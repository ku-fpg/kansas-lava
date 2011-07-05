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
-- Use these three if you *need* reset.
--    rst <= '1';
--   wait for clk_period / 2;
--    rst <= '0';
    while counter > 0 loop
      if (counter mod 100 = 0) then
        report("cycle: " & integer'image(cycle_count - counter));
      end if;
       counter := counter - 1;
      wait for clk_period / 2;
      clk <= '1';             -- rising edge
      wait for clk_period / 2;
      clk <= '0';             -- falling edge
    end loop;
    report "End of simulation." severity note;
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

-- A 'src' generator is a Master or Writer, the consumer is a Slave or Reader.
entity lava_src_tube is
  generic (
    src_file_name : string
  ); 
  port (
    M_VALID  : out std_logic;
    M_DATA   : out std_logic_vector(7 downto 0);
    M_READY  : in  std_logic;

    clk      : in std_logic;
    clk_en   : in std_logic := '1';
    rst      : in std_logic := '0'
  );
end entity;


architecture Behavioral of lava_src_pipe is
begin
  runtest: process is
    type char_file is file of character;
    file my_file : char_file;
    variable my_char_v : character; 
    variable my_byte : std_logic_vector(7 downto 0);
    variable buf_empty : boolean := true;  -- 1 element FIFO

    function str(b: boolean) return string is
    begin
       if b then
          return "true";
      else
        return "false";
       end if;
    end str;

  begin
    M_DATA <= (others => 'X');
    M_VALID <= '0';
    buf_empty := true;
    file_open(my_file, src_file_name, read_mode); 
    report("FILE: " & str(endfile(my_file)));

--    while (not endfile (my_file)) or (not buf_empty) loop
    while true loop
      -- Considerations are made on the rising edge
      wait until rising_edge(clk);

      -- if the previous packet was accepted, then empty the buffer token
      if M_READY = '1' then
        buf_empty := true;
      end if;

      -- if the buffer is empty, then fill it
      if buf_empty and not endfile(my_file) then
        report("READING");
	read(my_file, my_char_v); 
        report("READ: " & my_char_v);
	my_byte := std_logic_vector(to_unsigned(character'pos(my_char_v),8));
        buf_empty := false;
      end if;

      if buf_empty then
        M_DATA <= (others => 'X');
        M_VALID <= '0';
      else
        -- The buffer is now full, so send it
	M_DATA <= my_byte;
        M_VALID <= '1';
      end if;
    end loop;
--    wait until rising_edge(clk);    
--    -- The buffer is now empty
--    M_DATA <= (others => 'X');
--    M_VALID <= '0';
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


entity lava_sink_pipe is
  generic (
    sink_file_name : string
  ); 
  port (
    S_VALID  : in std_logic;
    S_DATA   : in std_logic_vector(7 downto 0);
    S_READY  : out  std_logic;

    clk      : in std_logic;
    clk_en   : in std_logic := '1';
    rst      : in std_logic := '0'
  );
end entity;


architecture Behavioral of lava_sink_pipe is
begin
  runtest: process is
    type char_file is file of character;
    file my_file : char_file;
    variable my_char_v : character; 
    variable my_byte : std_logic_vector(7 downto 0);
    variable buf_empty : boolean := true;  -- 1 element FIFO
  begin
    S_READY <= '1';                      -- Always ready
    while true loop
      -- Considerations are made on the rising edge
      wait until rising_edge(clk);

      -- if there is a value, then write it
      -- Very hacky, because ModelSim does not block if writing to a full pipe,
      -- we so need to open and close each time.
      if S_VALID = '1' then
        file_open(my_file, sink_file_name, append_mode); 
        my_char_v := character'val(to_integer(unsigned(S_DATA)));          
        write(my_file, my_char_v);
	file_close(my_file);
        report("WROTE: " & my_char_v);
      end if;
    end loop;
  end process;
end architecture;
