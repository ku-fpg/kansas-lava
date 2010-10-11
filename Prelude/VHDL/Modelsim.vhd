library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_textio.all;
library std;
use std.textio.all;
library work;

entity tb_env is
  generic (
    clk_period  : time 		:= 10 ns;  -- 100MHz
    cycle_count : integer 	:= 1000
); 
  port (
    clk      : out std_logic;
    clk_en   : out std_logic;
    rst      : out std_logic
);
end entity;

-- This is the vhdl/modelsim version of shallowEnv.
architecture Behavioral of tb_env is
begin
  runenv: process is
	variable counter : integer := cycle_count;
  begin
    clk <= '0';
    clk_en <= '1';
    rst <= '0';
    wait for clk_period / 2;
    while counter > 0 loop
    report("cycle: " & integer'image(cycle_count - counter));
    counter := counter - 1;
    wait for clk_period / 2;
    clk <= '1';             -- rising edge
    wait for clk_period / 2;
    clk <= '0';             -- falling edge
    end loop;
    wait;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_textio.all;
library std;
use std.textio.all;
library work;


entity lava_src_tube is
  generic (
    src_file_name : string
  ); 
  port (
    S_EXISTS : out std_logic;
    S_DATA   : out std_logic_vector(7 downto 0);
    S_READ   : in  std_logic;

    clk      : in std_logic;
    clk_en   : in std_logic := '1';
    rst      : in std_logic := '0'
  );
end entity;


architecture Behavioral of lava_src_tube is
begin
  runtest: process is
    type char_file is file of character;
    file my_file : char_file;
    variable my_char_v : character; 
    variable my_byte : std_logic_vector(7 downto 0);
    variable buf_empty : boolean := true;  -- 1 element FIFO
  begin
    S_DATA <= (others => 'X');
    S_EXISTS <= '0';
    buf_empty := true;
    file_open(my_file, src_file_name, read_mode); 
    while (not endfile (my_file)) or (not buf_empty) loop
      -- Considerations are made on the rising edge
      wait until rising_edge(clk);

      -- if the previous packet was accepted, then empty the buffer token
      if S_READ = '1' then
        buf_empty := true;
      end if;

      -- if the buffer is empty, then fill it
      if buf_empty then
	read(my_file, my_char_v); 
	my_byte := std_logic_vector(to_unsigned(character'pos(my_char_v),8));
        buf_empty := false;
      end if;

      -- The buffer is now full, so send it
      S_DATA <= my_byte;
      S_EXISTS <= '1';
    end loop;
    wait until rising_edge(clk);    
    -- The buffer is now empty
    S_DATA <= (others => 'X');
    S_EXISTS <= '0';
    wait;
  end process;
end architecture;
library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_textio.all;
library std;
use std.textio.all;
library work;


entity lava_sink_tube is
  generic (
    sink_file_name : string
  ); 
  port (
    M_WRITE  : in std_logic;
    M_DATA   : in std_logic_vector(7 downto 0);
    M_FULL   : out  std_logic;

    clk      : in std_logic;
    clk_en   : in std_logic := '1';
    rst      : in std_logic := '0'
  );
end entity;


architecture Behavioral of lava_sink_tube is
begin
  runtest: process is
    type char_file is file of character;
    file my_file : char_file;
    variable my_char_v : character; 
    variable my_byte : std_logic_vector(7 downto 0);
    variable buf_empty : boolean := true;  -- 1 element FIFO
  begin
    M_FULL <= '0';                      -- Always not full
    file_open(my_file, sink_file_name, write_mode); 
    while true loop
      -- Considerations are made on the rising edge
      wait until rising_edge(clk);

      -- if there is a value, then write it
      if M_WRITE = '1' then
        my_char_v := character'val(to_integer(unsigned(M_DATA)));          
        write(my_file, my_char_v);
      end if;
    end loop;
  end process;
end architecture;
