--
-- sdram.vhd
--
-- sdram controller implementation for the MiST board
-- http://code.google.com/p/mist-board/
-- 
-- Copyright (c) 2013 Till Harbaum <till@harbaum.org> 
-- 
-- This source file is free software: you can redistribute it and/or modify 
-- it under the terms of the GNU General Public License as published 
-- by the Free Software Foundation, either version 3 of the License, or 
-- (at your option) any later version. 
-- 
-- This source file is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of 
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License 
-- along with this program.  If not, see <http://www.gnu.org/licenses/>. 
--

-- TODO:
-- - optional 64 bit burst read
-- - setup of address+data earlier for increased stability?

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sdram is
    port
    (
        -- interface to the MT48LC16M16 chip
        sd_data         : inout std_ulogic_vector(15 downto 0);     -- 16 bit biriectional data bus
        sd_addr         : out std_ulogic_vector(12 downto 0);       -- 13 bit multiplexed address bus
        sd_dqm          : out std_ulogic_vector(1 downto 0);        -- two byte masks
        sd_ba           : out std_ulogic_vector(1 downto 0);        -- two banks
        sd_cs           : out std_ulogic;                           -- a single chip select
        sd_we           : out std_ulogic;                           -- write enable
        sd_ras          : out std_ulogic;                           -- row address select
        sd_cas          : out std_ulogic;                           -- column address select
        
        -- CPU/chipset interface
        init            : in std_ulogic;                            -- init signal after FPGA config to initialise RAM
        clk_128         : in std_ulogic;                            -- sdram is accessed at 128 MHz
        clk_8           : in std_ulogic;                            -- 8 MHz chipset clock to which SDRAM state machine is synchronised
        din             : in std_ulogic_vector(15 downto 0);        -- data input from chipset/CPU
        dout            : out std_ulogic_vector(63 downto 0);       -- data output to chipset/CPU
        addr            : in std_ulogic_vector(23 downto 0);        -- 24 bit address
        ds              : in std_ulogic_vector(1 downto 0);         -- upper/lower data strobe
        oe              : in std_ulogic;                            -- CPU/chipset requests read
        we              : in std_ulogic;                            -- CPU/chipset requests write)
        lb              : in std_ulogic                             -- long (128 bit) read burst when active
    );
end entity sdram;

architecture rtl of sdram is
    subtype int4 is integer range 0 to 15;

    constant RASCAS_DELAY       : int4 := 3;                        -- rRCD = 20 ns -> 3 cycles @128 MHz
    constant BURST_LENGTH       : std_ulogic_vector := "010";       -- 000=1, 001=2, 010=4, 011=8
    constant ACCESS_TYPE        : std_ulogic := '0';                -- 0=sequential, 1=interleaved
    constant CAS_LATENCY        : int4 := 3;                        -- 2/3 allowed
    constant OP_MODE            : std_ulogic_vector := "00";        -- only 00 (standard operation) allowed
    constant NO_WRITE_BURST     : std_ulogic := '1';                -- 0= write burst enabled, 1=only single access
    
    constant MODE               : std_ulogic_vector := "000" & 
                                                       NO_WRITE_BURST &
                                                       OP_MODE &
                                                       std_ulogic_vector(to_unsigned(CAS_LATENCY, 3)) &
                                                       ACCESS_TYPE &
                                                       BURST_LENGTH;
                                                       
    -- state machine definitions
    
    constant STATE_FIRST        : int4 := 0;
    constant STATE_CMD_START    : int4 := 1;
    constant STATE_CMD_CONT     : int4 := STATE_CMD_START + RASCAS_DELAY;
    constant STATE_READ         : int4 := STATE_CMD_CONT + CAS_LATENCY + 1;
    constant STATE_LAST         : int4 := 15;
    
    signal t                    : int4;
    signal reset                : integer range 0 to 31 := 0;
  
begin
    -----------------------------------------------------------------------
    -------------------------- cycle state machine ------------------------
    -----------------------------------------------------------------------

    -- The state machine runs at 128Mhz synchronous to the 8 Mhz chipset clock.
    -- It wraps from T15 to T0 on the rising edge of clk_8

    -- 128 MHz counter synchronous to 8 MHz clock
    -- force counter to pass state 0 exactly after the rising edge of clk_8
    
    p_counter : process
    begin
        wait until rising_edge(clk_128);
        if (t = STATE_LAST and clk_8 = '0') or
            ((t = STATE_FIRST) and (clk_8 = '1')) or
            ((t /= STATE_LAST) and (t /= STATE_FIRST)) then
            t <= t + 1;
        end if;
    end process p_counter;
    
    -- ---------------------------------------------------------------------
    -- --------------------------- startup/reset ---------------------------
    -- ---------------------------------------------------------------------

    -- wait 1ms (32 8Mhz cycles) after FPGA config is done before going
    -- into normal operation. Initialize the ram in the last 16 reset cycles (cycles 15-0)

    p_reset : process
    begin
        wait until rising_edge(clk_128);
        if init then
            reset <= 31;
        elsif t = STATE_LAST and reset /= 0 then
            reset <= reset - 1;
        end if;
    end process p_reset;
    
    sdram_sm : block
        -- all possible commands
        constant CMD_INHIBIT            : std_ulogic_vector := "1111";
        constant CMD_NOP                : std_ulogic_vector := "0111";
        constant CMD_ACTIVE             : std_ulogic_vector := "0011";
        constant CMD_READ               : std_ulogic_vector := "0101";
        constant CMD_WRITE              : std_ulogic_vector := "0100";
        constant CMD_BURST_TERMINATE    : std_ulogic_vector := "0110";
        constant CMD_PRECHARGE          : std_ulogic_vector := "0010";
        constant CMD_AUTO_REFRESH       : std_ulogic_vector := "0001";
        constant CMD_LOAD_MODE          : std_ulogic_vector := "0000";
        
        subtype burst_addr_type is integer range 0 to 3;
        signal sd_cmd                   : std_ulogic_vector(3 downto 0);    -- current command sent to SD RAM
        signal burst_addr               : burst_addr_type;
        signal data_latch               : std_ulogic_vector(15 downto 0);
    begin
        -- drive ram data lines when writing, set them as inputs otherwise
        sd_data <= din when we = '1' else (others => 'Z');
        
        sd_cs <= sd_cmd(3);
        sd_ras <= sd_cmd(2);
        sd_cas <= sd_cmd(1);
        sd_we <= sd_cmd(0);
        
        p_sm: process
        begin
            wait until rising_edge(clk_128);
            
            data_latch <= sd_data;
            
            sd_cmd <= CMD_INHIBIT;      -- default: idle
            
            if reset /= 0 then
                -- initialisation takes place at the end of the reset phase
                if t = STATE_CMD_START then
                    if reset = 13 then
                        sd_cmd <= CMD_PRECHARGE;
                        sd_addr(10) <= '1';             -- precharge all banks
                    end if;
                    
                    if reset = 2 then
                        sd_cmd <= CMD_LOAD_MODE;
                        sd_addr <= MODE;
                    end if;
                end if;
            else
                -- normal operation
                
                -- CPU/chipset read/write -------------------------------------------------
                if we = '1' or oe = '1' then
                    -- RAS phase
                    if t = STATE_CMD_START then
                        sd_cmd <= CMD_ACTIVE;
                        sd_addr <= "0" & addr(19 downto 8);
                        sd_ba <= addr(21 downto 20);
                        
                        -- always return both bytes in a read. The CPU may not need 
                        -- it, but the caches need to be able to store everything
                        if not we = '1' then
                            sd_dqm <= "00";
                        else
                            sd_dqm <= not ds;
                        end if;
                        
                        -- lowest address for burst read
                        burst_addr <= to_integer(unsigned(addr(1 downto 0)));
                    end if;
                    
                    -- CAS phase
                    if t = STATE_CMD_CONT then
                        if we = '1' then
                            sd_cmd <= CMD_WRITE;
                        else
                            sd_cmd <= CMD_READ;
                        end if;
                        sd_addr <= "0010" & addr(22) & addr(7 downto 0);        -- auto precharge
                    end if;
                    
                    -- read phase
                    if oe = '1' then
                        -- de-multiplex the data directly into the 64 bit buffer
                        if t >= STATE_READ + 1 and t < STATE_READ + 1 + 4 then
                            case burst_addr is
                                when 0 => dout(15 downto 0) <= data_latch;
                                when 1 => dout(31 downto 16) <= data_latch;
                                when 2 => dout(47 downto 32) <= data_latch;
                                when 3 => dout(63 downto 48) <= data_latch;
                            end case;
                            burst_addr <= burst_addr + 1;
                        end if;
                    end if;
                else
                    if t = STATE_CMD_START then
                        sd_cmd <= CMD_AUTO_REFRESH;
                    end if;
                end if;
            end if;
        end process p_sm;
    end block sdram_sm;
end architecture rtl;