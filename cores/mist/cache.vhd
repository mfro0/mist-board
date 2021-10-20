library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cache is
    port
    (
        clk_128         : in std_ulogic;
        clk             : in std_ulogic;
        reset           : in std_ulogic;
        flush           : in std_ulogic;
        
        strobe          : in std_ulogic;
        addr            : in std_ulogic_vector(22 downto 0);        -- CPU word address
        ds              : in std_ulogic_vector(1 downto 0);         -- upper (0) and lower (1) data strobe
        
        dout            : out std_ulogic_vector(15 downto 0);
        hit             : out std_ulogic;
        
        -- interface to store entire cache lines when read from ram
        din64           : in std_ulogic_vector(63 downto 0);
        store           : in std_ulogic;
        
        -- interface to update existing cache lines on CPU RAM write
        din16           : in std_ulogic_vector(15 downto 0);
        update          : in std_ulogic
    );
end entity cache;

architecture rtl of cache is
    -- cache size configuration
    -- the cache size in bytes is 8 * (2 ^ BITS), e.g. 2 kBytes if bits == 8
    constant BITS       : integer := 8;
    
    -- 8 bytes wide storage
    constant ENTRIES    : integer := 2 ** BITS;
    
    type dlatch_t is array(ENTRIES - 1 downto 0) of std_ulogic_vector(7 downto 0);
    type tlatch_t is array(ENTRIES - 1 downto 0) of std_ulogic_vector(21 - BITS - 1 downto 0);

    -- _word_ address mapping example with 16 cache lines (BITS == 4)
    -- 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    --  T  T  T  T  T  T  T  T  T  T  T  T  T  L  L  L  L  L  L  L  L  W  W
    -- T = stored in tag RAM
    -- L = cache line
    -- W = 16 bit word select 
    signal tag              : std_ulogic_vector(21 - BITS - 1 downto 0);
    signal lin              : integer range 0 to 2 ** BITS - 1;
    
    signal data_latch_7,
           data_latch_6,
           data_latch_5,
           data_latch_4,
           data_latch_3,
           data_latch_2,
           data_latch_1,
           data_latch_0     : dlatch_t;
    signal tag_latch        : tlatch_t;
    signal valid            : std_ulogic_vector(ENTRIES - 1 downto 0);
    signal current_tag      : std_ulogic_vector(21 - BITS - 1 downto 0);
    signal dout_latch_0,
           dout_latch_1,
           dout_latch_2,
           dout_latch_3     : std_ulogic_vector(15 downto 0);
    signal clear            : std_ulogic;

begin
    tag <= addr(22 downto 2 + BITS);
    lin <= to_integer(unsigned(addr(2 + BITS - 1 downto 2)));
    clear <= '1' when reset or flush else '0';
    
    
           
    -- signal indicating the currently selected cache line is valid and matches the
    -- address the CPU is currently requesting
    hit <= '1' when valid(lin) = '1' and (current_tag = tag) else '0';
    
    -- permanently output data according to current line
    -- de-multiplex 64 bit data into word requested by CPU
    with addr(1 downto 0) select dout <= dout_latch_0 when "00",
                                         dout_latch_1 when "01",
                                         dout_latch_2 when "10",
                                         dout_latch_3 when "11";
    
    p_dout_latch : process
    begin
        wait until rising_edge(clk_128);
        dout_latch_0 <= data_latch_1(lin) & data_latch_0(lin);
        dout_latch_1 <= data_latch_3(lin) & data_latch_2(lin);
        dout_latch_2 <= data_latch_5(lin) & data_latch_4(lin);
        dout_latch_3 <= data_latch_7(lin) & data_latch_6(lin);
        current_tag <= tag_latch(lin);
    end process p_dout_latch;
    
    p_cache : process(all)
    begin
        if clear = '1' then
            valid <= (others => '0');
        elsif rising_edge(clk) then
            if store = '1' then
                data_latch_7(lin) <= din64(63 downto 56);
                data_latch_6(lin) <= din64(55 downto 48);
                data_latch_5(lin) <= din64(47 downto 40);
                data_latch_4(lin) <= din64(39 downto 32);
                data_latch_3(lin) <= din64(31 downto 24);
                data_latch_2(lin) <= din64(23 downto 16);
                data_latch_1(lin) <= din64(15 downto 8);
                data_latch_0(lin) <= din64( 7 downto 0);
                
                tag_latch(lin) <= tag;
                valid(lin) <= '1';
            
            -- CPU (or other bus master) writes to RAM, so update cache contents if required
            elsif update = '1' and hit = '1' then
                -- no need to care for tag_latch or valid as they simply stay the same
                case addr(1 downto 0) is
                    when "00" =>
                        if ds(1) = '1' then data_latch_0(lin) <= din16( 7 downto 0); end if;
                        if ds(0) = '1' then data_latch_1(lin) <= din16(15 downto 8); end if;
                    when "01" =>
                        if ds(1) = '1' then data_latch_2(lin) <= din16( 7 downto 0); end if;
                        if ds(0) = '1' then data_latch_3(lin) <= din16(15 downto 8);
                        end if;
                    when "10" =>
                        if ds(1) = '1' then data_latch_4(lin) <= din16( 7 downto 0); end if;
                        if ds(0) = '1' then data_latch_5(lin) <= din16(15 downto 8); end if;
                    when "11" =>
                        if ds(1) = '1' then data_latch_6(lin) <= din16( 7 downto 0); end if;
                        if ds(0) = '1' then data_latch_7(lin) <= din16(15 downto 8); end if;
                end case;
            end if;
        end if;
    end process p_cache;
end architecture rtl;