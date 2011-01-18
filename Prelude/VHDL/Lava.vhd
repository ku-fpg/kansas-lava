-- These are core Lava built-in functions Lava programs can rely on having
-- Todo: Consider prepending lava_ to the names.

--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lava_bram is
        generic(
                addr_width : natural;
                data_width : natural;
                element_count : natural;
                sync : natural
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
  type mem_type is array (element_count-1 downto 0) of std_logic_vector(data_width-1 downto 0);
  signal mem : mem_type := (others => (others => 'X'));
begin
  proc : process(rst, clk, clk_en) is
  begin
    if rising_edge(clk) then
      if (clk_en = '1') then
        if (wEn = '1') then
           mem(to_integer(unsigned(wAddr))) <= wData;
        end if;
      end if;
    end if;
    if sync = 0 then
      -- async; someone else adding any delays on writing.
      o0 <= mem(to_integer(unsigned(rAddr)));    
    else
      if rising_edge(clk) then
        if (clk_en = '1') then
          -- sync; with built in delay
          o0 <= mem(to_integer(unsigned(rAddr)));
        end if;
      end if;
    end if;        
  end process proc;
        
end Behavioral;


--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lava_unsigned_mul is
        generic(
                width : natural
	);
        port (
                i0     : in std_logic_vector(width-1 downto 0);
                i1     : in std_logic_vector(width-1 downto 0);
                o0     : out std_logic_vector(width-1 downto 0)
        );
end entity lava_unsigned_mul;


architecture Behavioral of lava_unsigned_mul is
  signal tmp : std_logic_vector(2*width-1 downto 0);
begin
  -- a version of multiply that has the same sized output
  tmp <= std_logic_vector((unsigned(i0)) * (unsigned(i1)));
  o0 <= tmp(width-1 downto 0);
end Behavioral;

--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lava_signed_mul is
        generic(
                width : natural
	);
        port (
                i0     : in std_logic_vector(width-1 downto 0);
                i1     : in std_logic_vector(width-1 downto 0);
                o0     : out std_logic_vector(width-1 downto 0)
        );
end entity lava_signed_mul;


architecture Behavioral of lava_signed_mul is
  signal tmp : std_logic_vector(2*width-1 downto 0);
begin
  -- a version of multiply that has the same sized output
  tmp <= std_logic_vector((signed(i0)) * (signed(i1)));
  o0 <= tmp(width-1 downto 0);
end Behavioral;


--------------------------------------------------------------------------------
-- TO fix below this
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lava_sampled_add is
  generic (
    width : natural := 8;     -- signal width
    frac_width : natural := 8);      -- value for max * min 	
  port(i0 : in std_logic_vector(width-1 downto 0);
       i1 : in std_logic_vector(width-1 downto 0);
       o0 : out std_logic_vector(width-1 downto 0));
end entity lava_sampled_add;

architecture Behavioral of lava_sampled_add is
 signal tmp       : signed(width-1 + 1 downto 0);
 signal top2Bits : std_logic_vector(1 downto 0);
 constant zeros : std_logic_vector(width - 2 downto 0) := (others => '0');
 constant ones  : std_logic_vector(width - 2 downto 0) := (others => '1');
begin
  tmp <= signed (i0(width-1) & i0) + signed (i1(width-1) & i1);
  top2Bits <= std_logic_vector(tmp(width downto width-1));
  o0  <= '0' & ones when top2Bits = "01"  else
         '1' & zeros when top2Bits = "10" else
         std_logic_vector(tmp(width-1 downto 0));

end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


entity lava_sampled_sub is
  generic (
    width : natural := 8;     -- signal width
    frac_width : natural := 8);      -- value for max * min 	
  port(i0 : in std_logic_vector(width-1 downto 0);
       i1 : in std_logic_vector(width-1 downto 0);
       o0 : out std_logic_vector(width-1 downto 0));
end entity lava_sampled_sub;

architecture Behavioral of lava_sampled_sub is
 signal tmp       : signed(width-1 + 1 downto 0);
 constant zeros : std_logic_vector(width - 2 downto 0) := (others => '0');
 constant ones  : std_logic_vector(width - 2 downto 0) := (others => '1');
begin
  tmp <= signed (i0(width-1) & i0) - signed (i1(width-1) & i1);
  o0  <= '0' & ones when tmp(width downto width-1) = "01"  else
          '1' & zeros when tmp(width downto width-1) = "10" else
         std_logic_vector(tmp(width-1 downto 0));
end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lava_sampled_mul is
  generic (
    width : natural := 8;     -- signal width
    frac_width : natural := 4);      -- value for max * min 	
  port(i0 : in std_logic_vector(width-1 downto 0);
       i1 : in std_logic_vector(width-1 downto 0);
       o0 : out std_logic_vector(width-1 downto 0));
end entity lava_sampled_mul;

architecture Behavioral of lava_sampled_mul is
 signal tmp     : signed(2*width-1 downto 0);
 signal topBits : signed(width-1 downto 0);
 signal r0      : signed(2*width-1 downto frac_width);
 signal r1      : signed(2*width-1 downto frac_width);
 constant zeros : std_logic_vector(width - 2 downto 0) := (others => '0');
 constant ones  : std_logic_vector(width - 2 downto 0) := (others => '1');
begin
  tmp <= signed (i0) * signed (i1);
  r0 <= tmp(2*width-1 downto frac_width);
  -- This is Round half to even (http://en.wikipedia.org/wiki/Rounding#Round_half_to_even)
  r1 <= r0 when tmp(frac_width-1) = '0' else
        r0 when tmp(frac_width) = '0' and tmp(frac_width-1) = '1' and tmp (frac_width - 2 downto 0) = 0 else 
        r0 + 1;
  o0 <= std_logic_vector(r1(frac_width + width - 1 downto frac_width))
        when r1(2*width-1 downto frac_width + width-1) = 0 else
        std_logic_vector(r1(frac_width + width - 1 downto frac_width))
        when r1(2*width-1 downto frac_width + width-1) = -1 else
        '1' & zeros when tmp(2*width-1) = '1'  else '0' & ones;
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


entity lava_sampled_negate is
  generic (
    width : natural := 8;     -- signal width
    frac_width : natural := 8);      -- value for max * min 	
  port(i0 : in std_logic_vector(width-1 downto 0);
       o0 : out std_logic_vector(width-1 downto 0));
end entity lava_sampled_negate;

architecture Behavioral of lava_sampled_negate is
 signal tmp       : signed(width-1 + 1 downto 0);
 constant zeros : std_logic_vector(width - 2 downto 0) := (others => '0');
 constant ones  : std_logic_vector(width - 2 downto 0) := (others => '1');
begin
  tmp <= - signed (i0(width-1) & i0);
  o0  <= '0' & ones  when tmp(width downto width-1) = "01" else
          '1' & zeros when tmp(width downto width-1) = "10" else
         std_logic_vector(tmp(width-1 downto 0));
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sampled_fixedDivPowOfTwo is
  generic (
    width : natural := 8;     -- signal width
    frac_width : natural := 8;      -- value for max * min 	
    shift_by : natural);
  port(i0 : in std_logic_vector(width-1 downto 0);
       o0 : out std_logic_vector(width-1 downto 0));
end entity sampled_fixedDivPowOfTwo;

architecture Behavioral of sampled_fixedDivPowOfTwo  is
  signal t1 : std_logic_vector(width-1 downto 0);
  signal t2 : std_logic;
  signal t3 : std_logic_vector(width-1 downto 0);
  signal t4 : std_logic_vector(width-1 downto 0);
 constant zeros : std_logic_vector(width - 2 downto 0) := (others => '0');
 constant ones  : std_logic_vector(width - 2 downto 0) := (others => '1');
begin
  -- sign extend
  t1 <= (width-1 downto width-(shift_by +1) => i0(width-1)) & i0(width- shift_by downto shift_by);
  -- add on rounding
  t4 <= (width-1 downto 1 => '0') & (0 downto 0 => t2);
  -- This is Round half to even (http://en.wikipedia.org/wiki/Rounding#Round_half_to_even)
  t2 <= '0'    when i0(1 downto 0) = "00" else -- 0
        '0'    when i0(1 downto 0) = "01" else -- 0.25
         i0(2) when i0(1 downto 0) = "10" else -- 0.5  round up if odd, to even
        '1';                                   -- 0.75
  -- solution
  o0 <= std_logic_vector(signed(t1) + signed(t4));
end Behavioral;

