library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ostate_pkg is
  subtype mode_ty is std_logic_vector(1 downto 0);
  constant o_idle:  mode_ty := "10";
  constant o_busy:  mode_ty := "11";
  constant o_reset: mode_ty := "01";
end ostate_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package state_pkg is
  subtype state_ty is std_logic_vector(1 downto 0);
  constant resetState   : state_ty := "00";
  constant firstFill    : state_ty := "01";
  constant inputAndCount: state_ty := "10";
  constant result       : state_ty := "11";
end state_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package direction_pkg is
  subtype direction_ty is std_logic_vector(2 downto 0);
  constant N  : direction_ty := "010";
  constant S  : direction_ty := "011";
  constant E  : direction_ty := "000";
  constant W  : direction_ty := "001";

  constant NW : direction_ty := "100";
  constant NE : direction_ty := "110";
  constant SW : direction_ty := "111";
  constant SE : direction_ty := "101";
end direction_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;
use work.kirsch_synth_pkg.all;

entity kirsch is
  port (
    clk        : in  std_logic;                      
    reset      : in  std_logic;                      
    i_valid    : in  std_logic;                 
    i_pixel    : in  unsigned(7 downto 0);
    o_valid    : out std_logic;                 
    o_edge     : out std_logic;	                     
    o_dir      : out direction_ty;
    o_mode     : out mode_ty;
    o_row      : out unsigned(7 downto 0);
    o_col      : out unsigned(7 downto 0)
  );  
end entity kirsch;


architecture main of kirsch is
  signal state : state_ty := resetState;

  signal col_index  : unsigned(7 downto 0) := "00000000";
  signal row_index  : unsigned(7 downto 0) := "00000000";

  -- one-hot encoding to control the write enable of the mem entity
  signal row_wr_en  : unsigned(2 downto 0) := "000";

  signal o_ra : std_logic_vector(7 downto 0);
  signal o_rb : std_logic_vector(7 downto 0);
  signal o_rc : std_logic_vector(7 downto 0);
  signal o_rd : std_logic_vector(7 downto 0);
  signal o_re : std_logic_vector(7 downto 0);
  signal o_rf : std_logic_vector(7 downto 0);
  signal o_rg : std_logic_vector(7 downto 0);
  signal o_rh : std_logic_vector(7 downto 0);
  signal o_ri : std_logic_vector(7 downto 0);

  signal o_cal: integer;

  signal cnt:  unsigned(7 downto 0) := "00000000";
begin
  r1: entity WORK.mem
    port map (
      address => col_index,
      clock   => clk,
      data    => std_logic_vector(i_pixel),
      wren    => row_wr_en(0),
      q       => -- TODO: not sure what is Q for in this context
    );

  r2: entity WORK.mem
    port map (
      address => col_index,
      clock   => clk,
      data    => std_logic_vector(i_pixel),
      wren    => row_wr_en(1),
      q       => -- TODO: not sure what is Q for in this context
    );

  r3: entity WORK.mem
    port map (
      address => col_index,
      clock   => clk,
      data    => std_logic_vector(i_pixel),
      wren    => row_wr_en(2),
      q       => -- TODO: not sure what is Q for in this context
    );

process
begin
wait until rising_edge(clk);
  if (reset = '1') then
    -- this is in reset for both the mode and our internal statemachine
    state   <= resetState;
    o_valid <= '0';
    o_mode  <= o_reset;

    col_index <= to_unsigned(0, 8)
    row_index <= to_unsigned(0, 8)

    row_wr_en <= to_unsigned(1, 3)
  else
    case state is
      when resetState =>
      -- after the reset button is deasserted, go to the idle state
        o_mode <= o_idle;
        if (i_valid = '1') then 
          state <= firstFill;
          col_index <= col_index + 1;
        end if;

      when firstFill =>
      -- need to fill up to at least the first 2 row and the first 2 column of the third row
        if (i_valid) then
          -- at the end of row 0 and 1
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            col_index <= to_unsigned(0, 8);
            row_index <= row_index + 1;
          end if;

          -- finished filling up first 2 column on row 2
          if (col_index = to_unsigned(1, 8) and row_wr_en = to_unsigned(4, 3)) then
            state <= inputAndCount;
          end if;

          col_index <= col_index + 1;
        end if;

      when inputAndCount =>
      -- the core pipelined circuit
        if (i_valid) then
        end if;

      when result =>
      -- 
        o_data <= cnt;
        o_done <= '1';

        if (i_valid = '1') then
          state <= firstFill;
          col <= col + 1;
          cnt <= to_unsigned(0, 8);
        end if;

      when others =>

    end case;
  end if;

end process;
end architecture main;
