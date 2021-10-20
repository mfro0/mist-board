--
-- blitter.vhd
--
-- Atari ST BLiTTER implementation for the MiST board
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

-- blitter docs:
-- http://mikro.naprvyraz.sk/docs/ST_E/BLITTER.TXT
-- http://paradox.atari.org/files/BLIT_FAQ.TXT

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blitter is
    port
    (
        bus_cycle               : in std_ulogic_vector(1 downto 0);

        -- CPU register interface
        clk                     : in std_ulogic;
        reset                   : in std_ulogic;

        sel                     : in std_ulogic;
        addr                    : in std_ulogic_vector(4 downto 0);
        din                     : in std_ulogic_vector(15 downto 0);
        dout                    : out std_ulogic_vector(15 downto 0);
        uds,
        lds,
        rw                      : in std_ulogic;

        -- bus master interface
        bm_addr                 : out std_ulogic_vector(23 downto 1);
        bm_write,
        bm_read                 : out std_ulogic;
        bm_data_out             : out std_ulogic_vector(15 downto 0);
        bm_data_in              : in std_ulogic_vector(15 downto 0);

        br_in                   : in std_ulogic;
        br_out                  : out std_ulogic;
        irq                     : out std_ulogic;
        bg,
        turbo                   : in std_ulogic
    );
end entity blitter;

architecture rtl of blitter is

    type htr_t is array (15 downto 0) of std_ulogic_vector(15 downto 0);

    -- CPU controlled register set
    signal halftone_ram             : htr_t;
    signal src_x_inc, src_y_inc     : signed(15 downto 1);
    signal src_addr                 : unsigned(23 downto 1);

    signal endmask1,
           endmask2,
           endmask3                 : std_ulogic_vector(15 downto 0);
    signal dst_x_inc,
           dst_y_inc                : integer range -32768 to 32767;
    signal dst_addr                 : std_ulogic_vector(23 downto 1);

    signal x_count,
           x_count_latch            : integer range 0 to 2 ** 16 - 1;
    signal y_count                  : integer range 0 to 2 ** 16 - 1;
    signal hop                      : std_ulogic_vector(1 downto 0);
    signal op                       : std_ulogic_vector(3 downto 0);
    signal line_number,
           line_number_latch        : integer range 0 to 15;
    signal smudge,
           hog,
           busy                     : std_ulogic;

    signal skew                     : std_ulogic_vector(3 downto 0);
    signal nfsr,
           fxsr                     : std_ulogic;

    signal cycle_advance,
           cycle_read,
           cycle_advance_l,
           cycle_read_l             : std_ulogic;

    -- wire up the blitter subcomponent combinatorics
    signal src_skewed,
           src_halftoned,
           halftone_line,
           result                   : std_ulogic_vector(15 downto 0);
    signal no_src_hop               : std_ulogic;
    signal no_src_op,
           no_dest_op               : std_ulogic;
begin
    irq <= busy;

    -- specify which bus cycles to use
    cycle_advance <= '1' when bus_cycle = "00" or (turbo = '1' and bus_cycle = "10") else '0';
    cycle_read <= '1' when bus_cycle = "01" or (turbo = '1' and bus_cycle = "11");

    -- latch bus cycle information to use at the end of the cycle
    p_latch_bus_cycle : process
    begin
        wait until falling_edge(clk);
        cycle_advance_l <= cycle_advance;
        cycle_read_l <= cycle_read;
    end process p_latch_bus_cycle;

    -- --------------------------- CPU interface -----------------------------

    -- CPU read
    p_cpu_read : process(all)
        variable iaddr      : integer;
    begin
        dout <= (others => '0');
        iaddr := to_integer(unsigned(addr));

        if sel = '1' and rw = '1' then
            if iaddr >= 0 and iaddr <= 15 then dout <= halftone_ram(iaddr); end if;

            if iaddr = 16#10# then dout <= std_ulogic_vector(src_x_inc) & '0'; end if;
            if iaddr = 16#11# then dout <= std_ulogic_vector(src_y_inc) & '0'; end if;
            if iaddr = 16#12# then dout <= 8x"00" & std_ulogic_vector(src_addr(23 downto 16)); end if;
            if iaddr = 16#13# then dout <= std_ulogic_vector(src_addr(15 downto 1)) & '0'; end if;
            if iaddr = 16#14# then dout <= endmask1; end if;
            if iaddr = 16#15# then dout <= endmask2; end if;
            if iaddr = 16#16# then dout <= endmask3; end if;

            if iaddr = 16#17# then dout <= std_ulogic_vector(to_unsigned(dst_x_inc * 2, dout'length)); end if;
            if iaddr = 16#18# then dout <= std_ulogic_vector(to_unsigned(dst_y_inc * 2, dout'length)); end if;
            if iaddr = 16#19# then dout <= 8x"00" & dst_addr(23 downto 16); end if;
            if iaddr = 16#1a# then dout <= dst_addr(15 downto 1) & '0'; end if;
            if iaddr = 16#1b# then dout <= std_ulogic_vector(to_unsigned(x_count, 16)); end if;
            if iaddr = 16#1c# then dout <= std_ulogic_vector(to_unsigned(y_count, 16)); end if;

            -- since reading them has no side effect we can return the 8 bit registers
            -- without caring for uds/lds
            if iaddr = 16#1d# then dout <=  6x"0" & hop & 4x"0" & op; end if;
            if iaddr = 16#1e# then dout <= busy & hog & smudge & '0' &
                                           std_ulogic_vector(to_unsigned(line_number_latch, 4)) &
                                           fxsr & nfsr & "00" & skew; end if;
        end if;
    end process p_cpu_read;

    b_sm : block
        -- flag to initialise state machine
        signal init                 : std_ulogic;

        -- wait 1 bus cycle after bus has been requested to avoid that counters are updated before
        -- first bus transfer has taken place
        signal wait4bus             : std_ulogic;

        -- counter for cooperative (non-hog) bus access
        signal bus_coop_cnt         : integer range 0 to 2 ** 5 - 1;
        signal bus_owned            : std_ulogic;

        -- the state machine runs through most states for every word it processes
        -- state 0: normal source read cycle
        -- state 1: destination read cycle
        -- state 2: destination write cycle
        -- state 3: extra source read cycle (fxsr)
        signal state                : integer range 0 to 3;

        -- latch for read data
        signal bm_data_in_latch     : std_ulogic_vector(15 downto 0);

        signal x_count_next         : integer range 0 to 2 ** 16 - 1;

        signal skip_src_read,
               dest_required,
               next_dest_required   : std_ulogic;

        signal src                  : std_ulogic_vector(31 downto 0);
        signal dest                 : std_ulogic_vector(15 downto 0);

        signal first_word_in_row,
               last_word_in_row,
               next_is_first_word_in_row,
               next_is_last_word_in_row : std_ulogic;
        signal mask_requires_dest       : std_ulogic;

    begin
        p_latch : process
        begin
            wait until rising_edge(clk);
            if cycle_read_l then
                bm_data_in_latch <= bm_data_in;
            end if;
        end process p_latch;

        p_cpu_write : process(all)
            variable iaddr              : integer range 0 to 2 ** 5 - 1;
        begin
            iaddr := to_integer(unsigned(addr));

            -- blitter CPU register write interface -----------------------------
            if reset then
                busy <= '0';
                state <= 0;
                wait4bus <= '0';
            elsif falling_edge(clk) then
                if sel = '1' and not (rw = '0') then
                    -- 16/32 bit registers, not byte addressable
                    if iaddr >= 0 and iaddr <= 15 then halftone_ram(iaddr) <= din; end if;

                    if iaddr = 16#10# then src_x_inc <= signed(din(15 downto 1)); end if;
                    if iaddr = 16#11# then src_y_inc <= signed(din(15 downto 1)); end if;
                    if iaddr = 16#12# then src_addr(23 downto 16) <= unsigned(din(7 downto 0)); end if;
                    if iaddr = 16#13# then src_addr(15 downto 1) <= unsigned(din(15 downto 1)); end if;

                    if iaddr = 16#14# then endmask1 <= din; end if;
                    if iaddr = 16#15# then endmask2 <= din; end if;
                    if iaddr = 16#16# then endmask3 <= din; end if;

                    if iaddr = 16#17# then dst_x_inc <= to_integer(signed(din(15 downto 1))); end if;
                    if iaddr = 16#18# then dst_y_inc <= to_integer(signed(din(15 downto 1))); end if;
                    if iaddr = 16#19# then dst_addr(23 downto 16) <= din(7 downto 0); end if;
                    if iaddr = 16#1a# then dst_addr(15 downto 1) <= din(15 downto 1); end if;

                    if iaddr = 16#1b# then
                        x_count <= to_integer(unsigned(din));
                        x_count_latch <= to_integer(unsigned(din));   -- x_count is latched to be reloaded at each end of line
                    end if;

                    if iaddr = 16#1c# then y_count <= to_integer(unsigned(din)); end if;

                    -- ---------------- 8 bit registers -----------------------------------
                    -- uds -> even bytes via d(15 downto 8)
                    -- lds -> odd bytes via d(7 downto 0)
                    if iaddr = 16#1d# and not uds = '1' then hop <= din(9 downto 8); end if;
                    if iaddr = 16#1d# and not lds = '1' then op <= din(3 downto 0); end if;

                    if iaddr = 16#1e# and not uds = '1' then
                        line_number_latch <= to_integer(unsigned(din(11 downto 8)));
                        smudge <= din(13);
                        hog <= din(14);

                        -- writing busy with 1 starts the blitter, but only if y_count /= 0
                        if din(15) = '1' and y_count /= 0 then
                            busy <= '1';
                            wait4bus <= '1';
                            bus_coop_cnt <= 0;

                            -- initialise only if blitter is newly being started and not
                            -- if its already running
                            if not busy = '1' then init <= '1'; end if;

                            -- make sure the predicted x_count is one step ahead of the
                            -- real x_count
                            if x_count /= 1 then x_count_next <= x_count - 1; else
                                                 x_count_next <= x_count_latch; end if;
                        end if;
                    end if;

                    if iaddr = 16#1e# and not lds = '1' then
                        skew <= din(3 downto 0);
                        nfsr <= din(6);
                        fxsr <= din(7);
                    end if;
                end if;

                -- ----------------------------------------------------------------------------------
                -- -------------------------- blitter state machine ---------------------------------
                -- ----------------------------------------------------------------------------------

                -- entire state machine advances in bus_cycle 0
                -- (the cycle before the one being used by the cpu/blitter for memory access)

                -- grab bus if blitter is supposed to run (busy = '1') and we're not waiting for the bus
                if busy = '1' and (not wait4bus = '1' or (wait4bus = '1' and (bus_coop_cnt = 0))) then
                    br_out <= '1';
                else
                    br_out <= '0';
                end if;
                if busy = '1' and not wait4bus = '1' then
                    bus_owned <= '1';
                else
                    bus_owned <= '0';
                end if;

                -- clear busy flag if blitter is done
                if y_count = 0 then busy <= '0'; end if;

                -- the bus is freed/grabbed once this counter runs down to 0 in non-hog mode
                if busy = '1' and not hog = '1' and not br_in = '1' and bus_coop_cnt /= 0 then
                    bus_coop_cnt <= bus_coop_cnt - 1;
                end if;

                -- change between both states (bus grabbed and bus released)
                if bus_coop_cnt = 0 then
                    -- release bus immediately, grab bus only if bg is set
                    if not wait4bus = '1' or (wait4bus = '1' and bg = '1') then
                        wait4bus <= not wait4bus;
                    end if;
                end if;

                -- blitter has just been setup, so init the state machine in first step
                if init then
                    init <= '0';
                    line_number <= line_number_latch;

                    if skip_src_read = '1' then         -- skip source read (state 0)
                        if dest_required = '1' then
                            state <= 1;                 -- but dest needs to be read
                        else
                            state <= 2;                 -- also dest needs to be read
                        end if;
                    elsif fxsr = '1' then               -- first extra source read
                        state <= 3;
                    else
                        state <= 0;                      -- normal source read
                    end if;
                end if;

                -- advance state machine only if bus is owned
                if bus_owned = '1' and not br_in = '1' and (y_count /= 0) then
                    -- first extra source read (fxsr)
                    if state = 2 then
                        if src_x_inc < 0 then
                            src <= src(15 downto 0) & bm_data_in_latch;
                        else
                            src <= bm_data_in_latch & src(15 downto 0);
                        end if;
                        src_addr <= unsigned(signed(src_addr) + src_x_inc);
                        state <= 0;
                    end if;

                    if state = 0 then
                        -- don't do the last read of the last word in a row if nfsr is set
                        if nfsr = '1' and last_word_in_row = '1' then
                            -- no final source read, but shifting anyway
                            if src_x_inc > 0 then
                                src(31 downto 16) <= src(15 downto 0);
                            else
                                src(15 downto 0) <= src(31 downto 16);
                            end if;
                            src_addr <= unsigned(signed(src_addr) + src_y_inc);
                        else
                            if src_x_inc > 0 then
                                src <= src(15 downto 0) & bm_data_in_latch;
                            else
                                src <= bm_data_in_latch & src(15 downto 0);
                            end if;

                            if x_count /= 1 then
                                src_addr <= unsigned(signed(src_addr) + src_x_inc);
                            else
                                src_addr <= unsigned(signed(src_addr) + src_y_inc);
                            end if;
                        end if;

                        -- jump directly to destination write if no destination read is required
                        if dest_required = '1' then
                            state <= 1;
                        else
                            state <= 2;
                        end if;
                    end if;

                    if state = 1 then
                        dest <= bm_data_in_latch;
                        state <= 2;
                    end if;

                    if state = 2 then
                        -- y_count /= 0 means blitter is (still) active
                        if y_count /= 0 then
                            if x_count /= 1 then
                                -- we are at the beginning or within a line (have not reached the end yet)
                                dst_addr <= std_ulogic_vector(signed(dst_addr) + to_signed(dst_x_inc, dst_addr'length));
                                x_count <= x_count - 1;
                            else
                                -- we are at the end of a line but not finished yet
                                dst_addr <= std_ulogic_vector(signed(dst_addr) + to_signed(dst_y_inc, dst_addr'length));
                                if dst_y_inc >= 0 then
                                    line_number <= line_number + 1;
                                else
                                    line_number <= line_number - 1;
                                end if;
                                x_count <= x_count_latch;
                                y_count <= y_count - 1;
                            end if;
                            -- also advance the predicted next x_count
                            if x_count_next /= 1 then
                                x_count_next <= x_count_next - 1;
                            else
                                x_count_next <= x_count_latch;
                            end if;
                        end if;
                        if skip_src_read = '1' then
                            if next_dest_required then
                                state <= 1;
                            else
                                state <= 2;
                            end if;
                        elsif last_word_in_row = '1' and fxsr = '1' then
                            state <= 3;
                        else
                            state <= 0;
                        end if;
                    end if;
                end if;
            end if;
        end process p_cpu_write;

        -- source read takes place in state 0 (normal source read) and 3 (fxsr)
        bm_addr <= std_ulogic_vector(src_addr) when state = 0 or state = 3 else dst_addr;
        skip_src_read <= '1' when (no_src_hop = '1' or no_src_op = '1') and not smudge = '1' else '0';
        ------------------ blitter busmaster engine -----------------------------
        p_bl_busmaster : process
        begin
            wait until rising_edge(clk);
            bm_read <= '0';
            bm_write <= '0';

            if bus_owned = '1' and not br_in = '1' and y_count /= 0 and cycle_advance_l = '1' then
                if state = 0 then bm_read <= '1';
                elsif state = 1 then bm_read <= '1';
                elsif state = 2 then bm_write <= '1';
                elsif state = 3 then bm_read <= '1';    -- fxsr state
                end if;
            end if;
        end process p_bl_busmaster;

        halftone_line <= halftone_ram(to_integer(unsigned(src_skewed(3 downto 0)))) when smudge = '1' else halftone_ram(line_number);

        -- check if current column is first or last word in the row
        first_word_in_row <= '1' when x_count = x_count_latch else '0';
        last_word_in_row <= '1' when x_count = 1 else '0';

        -- check if next column is first or last word in the row
        next_is_first_word_in_row <= '1' when x_count_next = x_count_latch else '0';
        next_is_last_word_in_row <= '1' when x_count_next = 1 else '0';

        -- check if the current mask requires to read the destination first
        mask_requires_dest <= '1' when next_is_first_word_in_row = '1' and endmask1 /= 16x"ffff" else
                              '0' when next_is_first_word_in_row = '1' and endmask1 = 16x"ffff" else
                              '1' when next_is_last_word_in_row = '1' and endmask3 /= 16x"ffff" else
                              '0' when next_is_last_word_in_row = '1' and endmask3 = 16x"ffff" else
                              '1' when endmask2 /= 16x"ffff" else
                              '0';

        -- shift/select 16 bits of source
        i_shift : entity work.shift
            port map
            (
                skew        => skew,
                din         => src,
                dout        => src_skewed
            );

        -- apply halftone operation
        i_halftone_op : entity work.halftone_op
            port map
            (
                op          => hop,
                in0         => halftone_line,
                in1         => src_skewed,

                no_src      => no_src_hop,
                dout        => src_halftoned
            );

        -- apply blitter operation
        i_blitter_op : entity work.blitter_op
            port map
            (
                op          => op,
                in0         => src_halftoned,
                in1         => dest,

                no_src      => no_src_op,
                no_dest     => no_dest_op,
                dout        => result
            );

        -- apply masks

        i_masking : entity work.masking
            port map
            (
                endmask1    => endmask1,
                endmask2    => endmask2,
                endmask3    => endmask3,
                first       => first_word_in_row,
                last        => last_word_in_row,
                in0         => result,
                in1         => dest,
                dout        => bm_data_out
            );
    end block b_sm;
end architecture rtl;

------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity shift is
    port
    (
        skew            : in std_ulogic_vector(3 downto 0);
        din             : in std_ulogic_vector(31 downto 0);
        dout            : out std_ulogic_vector(15 downto 0)
    );
end entity shift;

architecture rtl of shift is
begin
    p_shift : process(all)
        variable iskew          : integer range 0 to 15;
    begin
        dout <= (others => '0');
        iskew := to_integer(unsigned(skew));
        case iskew is
            when 0 => dout <= din(15 downto 0);
            when 1 => dout <= din(16 downto 1);
            when 2 => dout <= din(17 downto 2);
            when 3 => dout <= din(18 downto 3);
            when 4 => dout <= din(19 downto 4);
            when 5 => dout <= din(20 downto 5);
            when 6 => dout <= din(21 downto 6);
            when 7 => dout <= din(22 downto 7);
            when 8 => dout <= din(23 downto 8);
            when 9 => dout <= din(24 downto 9);
            when 10 => dout <= din(25 downto 10);
            when 11 => dout <= din(26 downto 11);
            when 12 => dout <= din(27 downto 12);
            when 13 => dout <= din(28 downto 13);
            when 14 => dout <= din(29 downto 14);
            when 15 => dout <= din(30 downto 15);
        end case;
    end process p_shift;
end architecture rtl;

------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity halftone_op is
    port
    (
        op          : in std_ulogic_vector(1 downto 0);
        in0         : in std_ulogic_vector(15 downto 0);
        in1         : in std_ulogic_vector(15 downto 0);
        no_src      : out std_ulogic;
        dout        : out std_ulogic_vector(15 downto 0)
    );
end entity halftone_op;

architecture rtl of halftone_op is
begin
    p_halftone : process(all)
        variable iop    : integer range 0 to 3;
    begin
        iop := to_integer(unsigned(op));
        -- return 1 for all ops that don't use in1 (src)
        if iop = 0 or iop = 1 then no_src <= '1'; else no_src <= '0'; end if;

        case iop is
            when 0 => dout <= 16x"ffff";
            when 1 => dout <= in0;
            when 2 => dout <= in1;
            when 3 => dout <= in0 and in1;
        end case;
    end process p_halftone;
end architecture rtl;

------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blitter_op is
    port
    (
        op          : in std_ulogic_vector(3 downto 0);
        in0         : in std_ulogic_vector(15 downto 0);
        in1         : in std_ulogic_vector(15 downto 0);

        no_src      : out std_ulogic;
        no_dest     : out std_ulogic;
        dout        : out std_ulogic_vector(15 downto 0)
    );
end entity blitter_op;

architecture rtl of blitter_op is
begin
    p_blitter_op : process(all)
        variable iop        : integer;
    begin
        iop := to_integer(unsigned(op));

        -- return '1' for all ops that don't use in0 (src)
        if iop = 0 or iop = 5 or iop = 10 or iop = 15 then no_src <= '1'; else no_src <= '0'; end if;
        if iop = 0 or iop = 3 or iop = 12 or iop = 15 then no_dest <= '1'; else no_dest <= '0'; end if;

        case iop is
            when 0 => dout <= (others => '0');
            when 1 => dout <= in0 and in1;
            when 2 => dout <= in0 and not in1;
            when 3 => dout <= in0;
            when 4 => dout <= not in0 and in1;
            when 5 => dout <= in1;
            when 6 => dout <= in0 xor in1;
            when 7 => dout <= in0 or in1;
            when 8 => dout <= not in0 and not in1;
            when 9 => dout <= not in0 xor in1;
            when 10 => dout <= not in1;
            when 11 => dout <= in0 or not in1;
            when 12 => dout <= not in0;
            when 13 => dout <= not in0 or in1;
            when 14 => dout <= not in0 or not in1;
            when 15 => dout <= (others => '1');
            when others => null;
        end case;
    end process p_blitter_op;
end architecture rtl;

------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity masking is
    port
    (
        endmask1,
        endmask2,
        endmask3        : in std_ulogic_vector(15 downto 0);

        first,
        last            : in std_ulogic;

        in0,
        in1             : in std_ulogic_vector(15 downto 0);
        dout            : out std_ulogic_vector(15 downto 0)
    );
end entity masking;

architecture rtl of masking is
begin
    p_mask : process(all)
    begin
        -- neither first nor last: endmask2
        dout <= (in0 and endmask2) or (in1 and not endmask2);

        -- first (last may also be applied): endmask1
        if first = '1' then
            dout <= (in0 and endmask1) or (in1 and not endmask2);
        elsif last = '1' then
            dout <= (in0 and endmask3) or (in1 and not endmask3);
        end if;
    end process p_mask;
end architecture rtl;
