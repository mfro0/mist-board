library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity viking is
    port
    (
        pclk        : in std_ulogic;                        -- pixel clock
        
        -- memory interface
        himem       : in std_ulogic;                        -- use memory behind ROM
        bclk        : in std_ulogic;                        -- 8 MHz bus clock
        bus_cycle   : in std_ulogic_vector(1 downto 0);     -- bus cycle for bus access sync
        addr        : out std_ulogic_vector(22 downto 0);   -- video word address
        rd          : out std_ulogic;                       -- video read cycle
        data        : in std_ulogic_vector(63 downto 0);    -- video data read
        
        -- VGA output (multiplexed with sm124 output in top level)
        hs          : out std_ulogic;
        vs          : out std_ulogic;
        r,
        g,
        b           : out std_ulogic_vector(3 downto 0)
    );
end entity viking;

architecture rtl of viking is
    constant BASE       : std_ulogic_vector(22 downto 0) := 23x"60_0000";   -- 0xc00000
    constant BASE_HI    : std_ulogic_vector(22 downto 0) := 23x"74_0000";   -- 0xe80000
    
    -- total width must be multiple of 64, so video runs synchronous
    -- to main bus
    --
    -- Horizontal timing
    -- HBP1 |                    H              | HFP | HSS| HBP2
    -- -----|XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX|-----|____|-----
    -- HBP1 is used for prefetch
	
    -- 1280x

    type int10 is range 0 to 2 ** 11 - 1;

    constant H      : int10 := 1280;
    constant HFP    : int10 := 88;
    constant HSS    : int10 := 136;
    constant HBP1   : int10 := 32;
    constant HBP2   : int10 := 192;

    -- Vertical timing
    --                     V              | VFP | VSS| VBP
    -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX|-----|____|-----

    -- x1024
    constant V      : int10 := 1024;
    constant VFP    : int10 := 9;
    constant VSS    : int10 := 4;
    constant VBP    : int10 := 9;


    signal v_cnt            : int10;
    signal h_cnt            : int10;
    
    signal bus_cycle_l      : std_ulogic_vector(5 downto 0);
    signal me               : std_ulogic;
    signal de               : std_ulogic;
begin
    -- memory enable (data is being read from memory)
    me <= '1' when (v_cnt < V) and (h_cnt < H) else '0';
    
    -- display enable (data is being displayed)
    de	<= '1' when (v_cnt < V) and (h_cnt >= HBP1) and (h_cnt < HBP1 + H) else '0';	

    -- memory enable can directly be used as a ram read signal
    rd <= '1' when unsigned(bus_cycle) = 2 and me = '1' else '0';

    -- ---------------------------------------------------------------------------
    -- --------------------------- internal state counter ------------------------
    -- ---------------------------------------------------------------------------

    internal_state_counter: block
        signal t    : unsigned(3 downto 0);
    begin
        p_counter : process
        begin
            wait until rising_edge(pclk);
            -- 128 Mhz counter synchronous to 8 Mhz clock
            -- force counter to pass state 0 exactly after the rising edge of clk_reg (8Mhz)
            if (t = 15 and bclk = '0') or
               (t = 0  and bclk = '1') or
               (t /= 15 and (t /= 0)) then
                t <= t + 1;
            end if;
        end process p_counter;
        
        -- create internal bus_cycle signal which is stable on the positive clock
        -- edge and extends the previous state by half a 128 Mhz clock cycle
        p_bus_cycle : process
        begin
            wait until falling_edge(pclk);
            bus_cycle_l <= bus_cycle & std_ulogic_vector(t);
        end process p_bus_cycle;
    end block internal_state_counter;
    
    timing : block
        signal input_latch      : std_ulogic_vector(63 downto 0);
        signal shift_register   : std_ulogic_vector(63 downto 0);
        signal pix              : std_ulogic;
        signal pix4             : std_ulogic_vector(3 downto 0);
    begin
        -- --------------- horizontal timing -------------
        hs <= '0' when h_cnt >= HBP1 + H + HFP and 
                       h_cnt < HBP1 + H + HFP + HSS else '1';
        p_hwrap : process
        begin
            wait until rising_edge(pclk);
            if h_cnt = HBP1 + H + HFP + HSS + HBP2 - 1 then
                -- make sure a line starts with the "video" bus cycle (0)
                -- CPU has cycles 1 and 3
                if bus_cycle_l = 2d"1" & 4d"15" then
                    h_cnt <= 0;
                end if;
            else
                h_cnt <= h_cnt + 1;
            end if;
        end process p_hwrap;
        
        -- --------------- vertical timing -------------
        vs <= '0' when v_cnt >= V + VFP and 
                       v_cnt < V + VFP + VSS else '1';
        p_vwrap : process
        begin
            wait until rising_edge(pclk);
            if h_cnt = HBP1 + H + HFP + HSS + HBP2 - 1 then
                if v_cnt = V + VFP + VSS + VBP - 1 then
                    v_cnt <= 0;
                else
                    v_cnt <= v_cnt + 1;
                end if;
            end if;
        end process p_vwrap;
        
        -- ---------------- memory timing ----------------
        p_memory_timing : process
        begin
            wait until rising_edge(pclk);
            -- last line on screen
            if v_cnt = V + VFP + VSS + VBP - 2 then
                if himem = '1' then
                    addr <= BASE_HI;
                else
                    addr <= BASE;
                end if;
            elsif me = '1' and bus_cycle_l = 6x"30" then
                addr <= std_ulogic_vector(unsigned(addr) + 23d"4");
            end if;
            
            if me = '1' and bus_cycle_l = 6x"2f" then
                input_latch <= data;
            end if;
            
            if bus_cycle_l = 6x"3f" then
                -- reorder words 1:2:3:4 -> 4:3:2:1
                shift_register <= input_latch(15 downto 0) &
                                  input_latch(31 downto 16) &
                                  input_latch(47 downto 32) &
                                  input_latch(63 downto 48);
            else
                shift_register(63 downto 1) <= shift_register(62 downto 0);
            end if;
        end process p_memory_timing;
        
        pix <= not shift_register(63) when de = '1' else '0';
        pix4 <= pix & pix & pix & pix;
        
        r <= pix4;
        g <= pix4;
        b <= pix4;
    end block timing;
end architecture rtl;
