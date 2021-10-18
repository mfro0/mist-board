//
// video.v
// 
// Copyright (c) 2015 Till Harbaum <till@harbaum.org> 
// 
// This source file is free software: you can redistribute it and/or modify 
// it under the terms of the GNU General Public License as published 
// by the Free Software Foundation, either version 3 of the License, or 
// (at your option) any later version. 
// 
// This source file is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of 
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License 
// along with this program.  If not, see <http://www.gnu.org/licenses/>. 

module video (
  // system interface
  input 	clk_128,     // 127.5 MHz
  input 	clk_32,      // 31.875 MHz
  input [1:0] 	bus_cycle,   // bus-cycle for sync

  // SPI interface for OSD
  input 	sck,
  input 	ss,
  input 	sdi,

  // memory interface
  output [22:0] vaddr, // video word address counter
  output 	read, // video read cycle
  input [63:0] 	data, // video data read
  
  // cpu register interface
  input 	cpu_clk,
  input 	cpu_reset,
  input [15:0] 	cpu_din,
  input 	cpu_sel,
  input [5:0] 	cpu_addr,
  input 	cpu_uds,
  input 	cpu_lds,
  input 	cpu_rw,
  output [15:0] cpu_dout,
  
  // debug overlay
  input dbg_enable,
  input [31:0] dbg_val_a,
  input [31:0] dbg_val_d,
  input [31:0] dbg_val_s,
  
  // screen interface
  output 	hs,                  // H_SYNC
  output 	vs,                  // V_SYNC
  output [5:0] 	video_r,             // Red[5:0]
  output [5:0] 	video_g,             // Green[5:0]
  output [5:0] 	video_b,             // Blue[5:0]

  // system config
  input         viking_enable,       // enable viking video card
  input         viking_himem,        // let viking use memory from $e80000
  input 	scandoubler_disable,        // don't use scandoubler in 15khz modes 
  input 	ypbpr,                      // output ypbpr instead of rgb 
  input 	pal56,               // use VGA compatible 56hz for PAL
  input [1:0] 	scanlines,           // scanlines (00-none 01-25% 10-50% 11-100%)
  input [15:0] 	adjust,              // hor/ver video adjust
  input 	ste,                 // enable STE featurss
 
  // signals not affected by scan doubler for internal use like irqs
  output 	st_de,
  output 	st_vs,
  output 	st_hs 
);

// give viking access to the memory if it's enabled
assign vaddr = viking_enable?viking_vaddr:shifter_vaddr;
assign read  = viking_enable?viking_read:shifter_read;

wire ypbpr_cs = ~(shifter_sd_adjusted_hs ^ shifter_sd_adjusted_vs);

// if we use 15khz signals without scan doubler then we need
// to create a composite sync on hsync
wire enable_csync =  sd_15khz_detected && scandoubler_disable;
// wire csync = shifter_hs == shifter_vs;
wire csync = shifter_sd_adjusted_hs == shifter_sd_adjusted_vs;
assign hs = enable_csync?csync:ypbpr?ypbpr_cs:stvid_hs;
assign vs = (enable_csync || ypbpr)?1'b1:stvid_vs;

// ------------------------- OSD ---------------------------

// in viking mode OSD is operated at 64 MHz pixel clock
reg clk_64;
always @(posedge clk_128)
  clk_64 <= !clk_64;

wire osd_clk = viking_enable?clk_128:clk_32;
   
wire [5:0] y, pb, pr;
rgb2ypbpr rgb2ypbpr (
	.red    ( osd_r ),
	.green  ( osd_g ),
	.blue   ( osd_b ),
	
	.y      ( y     ),
	.pb     ( pb    ),
	.pr     ( pr    )
);

// demultiplex between ypbpr and rgb signals
assign video_r = ypbpr?pr:osd_r;
assign video_g = ypbpr? y:osd_g;
assign video_b = ypbpr?pb:osd_b;

// include OSD overlay
wire [5:0] osd_r, osd_g, osd_b;
osd osd (
         .clk        ( osd_clk    ),

         // OSD spi interface to io controller
         .sdi        ( sdi        ),
         .sck        ( sck        ),
         .ss         ( ss         ),

         // feed ST video signal into OSD
			.hs         ( stvid_hs ),
			.vs         ( stvid_vs ),
	 
         .r_in       ( ovl_r ),
         .g_in       ( ovl_g ),
         .b_in       ( ovl_b ),
	 
         // receive signal with OSD overlayed
         .r_out      ( osd_r  ),
         .g_out      ( osd_g  ),
         .b_out      ( osd_b  )
);

// include debug overlay
wire [5:0] ovl_r, ovl_g, ovl_b;
overlay overlay (
         .pclk       ( clk_32       ),
			.enable     ( dbg_enable   ),

			.val_a      ( dbg_val_a    ),
			.val_d      ( dbg_val_d    ),
			.val_s      ( dbg_val_s    ),

         .red_in     ( stvid6_r     ),
         .green_in   ( stvid6_g     ),
         .blue_in    ( stvid6_b     ),
			.hs_in      ( stvid_hs     ),
			.vs_in      ( stvid_vs     ),

         // receive signal with OSD overlayed
         .red_out    ( ovl_r        ),
         .green_out  ( ovl_g        ),
         .blue_out   ( ovl_b        )
);

// expand ST(E) 3/4 bit colors to MISTs 6 bit range
// in 3 bit ST mode we can simply double each bit to achieve 6 bits
// in 4 bit STE mode the mapping isn't linear
wire [5:0] stvid6_r = ste?{stvid_r, stvid_r[3:2]}:{stvid_r[3:1],stvid_r[3:1]};
wire [5:0] stvid6_g = ste?{stvid_g, stvid_g[3:2]}:{stvid_g[3:1],stvid_g[3:1]};
wire [5:0] stvid6_b = ste?{stvid_b, stvid_b[3:2]}:{stvid_b[3:1],stvid_b[3:1]};

// ------------- combine scandoubled shifter with viking -------------
wire [3:0] stvid_r = viking_enable?viking_r:shifter_sd_sblank_r;
wire [3:0] stvid_g = viking_enable?viking_g:shifter_sd_sblank_g;
wire [3:0] stvid_b = viking_enable?viking_b:shifter_sd_sblank_b;
wire stvid_hs = viking_enable?viking_hs:vga_hs;
wire stvid_vs = viking_enable?viking_vs:vga_vs;

// -------- make sure adjusted video blanks during sync phase -------
wire n_sync_adjusted = shifter_sd_adjusted_hs && shifter_sd_adjusted_vs;
wire [3:0] shifter_sd_sblank_r = n_sync_adjusted?shifter_sd_r:4'b0000;
wire [3:0] shifter_sd_sblank_g = n_sync_adjusted?shifter_sd_g:4'b0000;
wire [3:0] shifter_sd_sblank_b = n_sync_adjusted?shifter_sd_b:4'b0000;
   
// --------------- apply screen position adjustments -----------------

// apply vga sync polarity adjustment to scan doubler output. It doesn't hurt
// to do this even if 15khz modes are being used since the 15khz modes generate
// their csync signals from other signals
wire vga_hs = shifter_sd_adjusted_hs ^ vga_hs_pol;
wire vga_vs = shifter_sd_adjusted_vs ^ vga_vs_pol;

wire shifter_sd_adjusted_hs;
wire shifter_sd_adjusted_vs;   
   
sync_adjust sync_adjust (
       .clk    ( clk_32                 ),
       .adjust ( adjust                 ),

       .hs_in  ( shifter_sd_hs          ),
       .vs_in  ( shifter_sd_vs          ),

       .hs_out ( shifter_sd_adjusted_hs ),
       .vs_out ( shifter_sd_adjusted_vs )
);
   
// --------------- combine shifter with scan doubler -----------------

// use scandoubler if 15khz signal has been detected and 
// scandoubler isn't disabled
wire use_scandoubler = sd_15khz_detected && !scandoubler_disable;
   
// forward scandoubled signals whenever scandouble is to be used
wire [3:0] shifter_sd_r = use_scandoubler?sd_r:shifter_r;
wire [3:0] shifter_sd_g = use_scandoubler?sd_g:shifter_g;
wire [3:0] shifter_sd_b = use_scandoubler?sd_b:shifter_b;
wire    shifter_sd_hs = use_scandoubler?sd_hs:shifter_hs;
wire    shifter_sd_vs = use_scandoubler?sd_vs:shifter_vs;
   
// --------------- the scan doubler for 15khz modes -----------------
wire sd_15khz_detected;
wire sd_hs, sd_vs;
wire [3:0] sd_r, sd_g, sd_b;

scandoubler scandoubler (
	 .clk       ( clk_32     ), // 33.000 MHz
	 .clk_16    ( clk_16     ),

	 .scanlines ( scanlines  ),
			 
	 // video input from shifter
	 .hs_in     ( shifter_hs ),
	 .vs_in     ( shifter_vs ),
	 .r_in      ( shifter_r  ),
	 .g_in      ( shifter_g  ),
	 .b_in      ( shifter_b  ),

	 // output interface
	 .hs_out    ( sd_hs      ),
	 .vs_out    ( sd_vs      ),
	 .r_out     ( sd_r       ),
	 .g_out     ( sd_g       ),
	 .b_out     ( sd_b       ),
			 
	 .is15k     ( sd_15khz_detected )
);

// --------------- the Atari ST(E) shifter chip -----------------
wire shifter_hs, shifter_vs;
wire [3:0] shifter_r, shifter_g, shifter_b;

wire [22:0] shifter_vaddr;
wire shifter_read;

// sync polarity to be used when outputting to VGA
wire vga_hs_pol, vga_vs_pol;
   
// only use pal56 modes if the scandoubler is being used
wire use_pal56 = pal56 && !scandoubler_disable;

wire clk_16;
   
shifter shifter (
	 .clk       ( clk_32        ), // 31.875 MHz
	 .bus_cycle ( bus_cycle     ), // to sync memory access with cpu
		 
	 // memory interface
	 .vaddr     ( shifter_vaddr ), // video word address
	 .read      ( shifter_read  ), // video read cycle
	 .data      ( data          ), // video data read
  
	 // cpu register interface
	 .cpu_clk   ( cpu_clk       ),
	 .cpu_reset ( cpu_reset     ),
	 .cpu_din   ( cpu_din       ),
	 .cpu_sel   ( cpu_sel       ),
	 .cpu_addr  ( cpu_addr      ),
	 .cpu_uds   ( cpu_uds       ),
	 .cpu_lds   ( cpu_lds       ),
	 .cpu_rw    ( cpu_rw        ),
	 .cpu_dout  ( cpu_dout      ),
  
	 // screen interface
	 .hs        ( shifter_hs    ), // H_SYNC
	 .vs        ( shifter_vs    ), // V_SYNC
	 .video_r   ( shifter_r     ), // Red[5:0]
	 .video_g   ( shifter_g     ), // Green[5:0]
	 .video_b   ( shifter_b     ), // Blue[5:0]

	 // sync polarity to be used on vga
	 .vga_vs_pol ( vga_vs_pol   ),
	 .vga_hs_pol ( vga_hs_pol   ),
	 .clk_16     ( clk_16       ),

	 // system config
	 .pal56     ( use_pal56     ), // use VGA compatible 56hz for PAL
	 .ste       ( ste           ), // enable STE features
 
	 // signals not affected by scan doubler for internal use like irqs
	 .st_de     ( st_de         ),
	 .st_vs     ( st_vs         ),
	 .st_hs     ( st_hs         )
);

// --------------- the Viking compatible 1280x1024 graphics card -----------------
wire viking_hs, viking_vs;
wire [3:0] viking_r, viking_g, viking_b;

wire [22:0] viking_vaddr;
wire viking_read;

viking viking (
       .pclk      ( clk_128         ),   // 128MHz
       .himem     ( viking_himem    ),
       .bclk      ( cpu_clk         ),
       .bus_cycle ( bus_cycle       ), // bus-cycle to sync video memory access with cpu
	       
       // memory interface
       .addr      ( viking_vaddr    ), // video word address
       .rd      ( viking_read     ), // video read cycle
       .data      ( data            ), // video data read

       // video output
       .hs        ( viking_hs       ),
       .vs        ( viking_vs       ),
       .r         ( viking_r        ),
       .g         ( viking_g        ),
       .b         ( viking_b        )
);

endmodule
