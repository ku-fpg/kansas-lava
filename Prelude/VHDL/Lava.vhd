-- These are core Lava built-in functions Lava programs can rely on having
-- Todo: Consider prepending lava_ to the names.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lava_register is
        generic(
                width : natural
        );
        port(
                rst : in std_logic;
                clk : in std_logic;
                clk_en : in std_logic;
                i0 : in std_logic_vector(width-1 downto 0);
                def : in std_logic_vector(width-1 downto 0);
                o0 : out std_logic_vector(width-1 downto 0)
        );
end entity lava_register;

architecture Behavioral of lava_register is
begin
  proc : process(rst, clk, clk_en) is
  begin
    if rst = '1' then
        o0 <= def;
    elsif rising_edge(clk) then
      if (clk_en = '1') then
        o0 <= i0;
      end if;
    end if;
  end process proc;
end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lava_bit_register is
        port(
                rst : in std_logic;
                clk : in std_logic;
                clk_en : in std_logic;
		i0 : in std_logic;
		def : in std_logic;
                o0 : out std_logic
        );
end entity lava_bit_register;

architecture Behavioral of lava_bit_register is
begin
  proc : process(rst, clk, clk_en) is
  begin
    if rst = '1' then
        o0 <= def;
    elsif rising_edge(clk) then
      if (clk_en = '1') then
        o0 <= i0;
      end if;
    end if;
  end process proc;
end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lava_bram is
        generic(
                addr_width : natural;
                data_width : natural;
                element_count : natural
        );
        port (
                rst    : in std_logic;
                clk    : in std_logic;
                clk_en : in std_logic;
                wEn    : in std_logic;
                wAddr  : in std_logic_vector(addr_width-1 downto 0);
                wData  : in std_logic_vector(data_width-1 downto 0);
                rAddr  : in std_logic_vector(addr_width-1 downto 0);
                o0     : out std_logic_vector(data_width-1 downto 0)
        );
end entity lava_bram;

architecture Behavioral of lava_bram is
begin
  proc : process(rst, clk, clk_en) is
  begin
    if rising_edge(clk) then
      if (clk_en = '1') then
        o0 <= wData;
      end if;
    end if;
  end process proc;
end Behavioral;



library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.all;

entity Sampled_addition is
  generic (
    width_size : natural := 8;     -- signal width
    max_value : natural := 8);      -- value for max * min 	
  port(i0 : in std_logic_vector(width_size-1 downto 0);
       i1 : in std_logic_vector(width_size-1 downto 0);
       o0 : out std_logic_vector(width_size-1 downto 0));
end entity Sampled_addition;

architecture Behavioral of Sampled_addition is
 signal tmp       : signed(width_size-1 + 1 downto 0);
 signal top2Bits : std_logic_vector(1 downto 0);
 constant zeros : std_logic_vector(width_size - 2 downto 0) := (others => '0');
 constant ones  : std_logic_vector(width_size - 2 downto 0) := (others => '1');
begin
  tmp <= signed (i0(width_size-1) & i0) + signed (i1(width_size-1) & i1);
  top2Bits <= std_logic_vector(tmp(width_size downto width_size-1));
  o0  <= '0' & ones when top2Bits = "01"  else
         '1' & zeros when top2Bits = "10" else
         std_logic_vector(tmp(width_size-1 downto 0));

end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.all;

entity Sampled_greaterThan is
  generic (
    width_size : natural := 8;     -- signal width
    max_value : natural := 8);      -- value for max * min 	
  port(i0 : in std_logic_vector(width_size-1 downto 0);
       i1 : in std_logic_vector(width_size-1 downto 0);
       o0 : out std_logic);
end entity Sampled_greaterThan;

architecture Behavioral of Sampled_greaterThan is
begin
  o0  <= '1' when (signed(i0) > signed(i1)) else '0';
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.all;


entity Sampled_subtraction is
  generic (
    width_size : natural := 8;     -- signal width
    max_value : natural := 8);      -- value for max * min 	
  port(i0 : in std_logic_vector(width_size-1 downto 0);
       i1 : in std_logic_vector(width_size-1 downto 0);
       o0 : out std_logic_vector(width_size-1 downto 0));
end entity Sampled_subtraction;

architecture Behavioral of Sampled_subtraction is
 signal tmp       : signed(width_size-1 + 1 downto 0);
 constant zeros : std_logic_vector(width_size - 2 downto 0) := (others => '0');
 constant ones  : std_logic_vector(width_size - 2 downto 0) := (others => '1');
begin
  tmp <= signed (i0(width_size-1) & i0) - signed (i1(width_size-1) & i1);
  o0  <= '0' & ones when tmp(width_size downto width_size-1) = "01"  else
          '1' & zeros when tmp(width_size downto width_size-1) = "10" else
         std_logic_vector(tmp(width_size-1 downto 0));
end Behavioral;



library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.all;


entity Sampled_negate is
  generic (
    width_size : natural := 8;     -- signal width
    max_value : natural := 8);      -- value for max * min 	
  port(i0 : in std_logic_vector(width_size-1 downto 0);
       o0 : out std_logic_vector(width_size-1 downto 0));
end entity Sampled_negate;

architecture Behavioral of Sampled_negate is
 signal tmp       : signed(width_size-1 + 1 downto 0);
 constant zeros : std_logic_vector(width_size - 2 downto 0) := (others => '0');
 constant ones  : std_logic_vector(width_size - 2 downto 0) := (others => '1');
begin
  tmp <= - signed (i0(width_size-1) & i0);
  o0  <= '0' & ones  when tmp(width_size downto width_size-1) = "01" else
          '1' & zeros when tmp(width_size downto width_size-1) = "10" else
         std_logic_vector(tmp(width_size-1 downto 0));
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.all;

entity sampled_fixedDivPowOfTwo is
  generic (
    width_size : natural := 8;     -- signal width
    max_value : natural := 8;      -- value for max * min 	
    shift_by : natural);
  port(i0 : in std_logic_vector(width_size-1 downto 0);
       o0 : out std_logic_vector(width_size-1 downto 0));
end entity sampled_fixedDivPowOfTwo;

architecture Behavioral of sampled_fixedDivPowOfTwo  is
  signal t1 : std_logic_vector(width_size-1 downto 0);
  signal t2 : std_logic;
  signal t3 : std_logic_vector(width_size-1 downto 0);
  signal t4 : std_logic_vector(width_size-1 downto 0);
 constant zeros : std_logic_vector(width_size - 2 downto 0) := (others => '0');
 constant ones  : std_logic_vector(width_size - 2 downto 0) := (others => '1');
begin
  -- sign extend
  t1 <= (width_size-1 downto width_size-(shift_by +1) => i0(width_size-1)) & i0(width_size- shift_by downto shift_by);
  -- add on rounding
  t4 <= (width_size-1 downto 1 => '0') & (0 downto 0 => t2);
  -- This is Round half to even (http://en.wikipedia.org/wiki/Rounding#Round_half_to_even)
  t2 <= '0'    when i0(1 downto 0) = "00" else -- 0
        '0'    when i0(1 downto 0) = "01" else -- 0.25
         i0(2) when i0(1 downto 0) = "10" else -- 0.5  round up if odd, to even
        '1';                                   -- 0.75
  -- solution
  o0 <= std_logic_vector(signed(t1) + signed(t4));
end Behavioral;

