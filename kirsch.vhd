library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ostate_pkg is
  subtype mode_ty is std_logic_vector(1 downto 0);
  constant o_idle:  mode_ty := "10";
  constant o_busy:  mode_ty := "11";
  constant o_reset: mode_ty := "01";

  subtype state_ty is std_logic_vector(1 downto 0);
  constant resetState   : state_ty := "00";
  constant firstFill    : state_ty := "01";
  constant inputAndCount: state_ty := "10";
  constant result       : state_ty := "11";

  subtype cal_state_ty is std_logic_vector(2 downto 0);
  constant cycle_00 : state_ty := "00";
  constant cycle_01 : state_ty := "01";
  constant cycle_02 : state_ty := "10";
  constant cycle_03 : state_ty := "11";
end ostate_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;
use work.kirsch_synth_pkg.all;
use work.ostate_pkg.all;

entity kirsch is
  port (
    clk        : in  std_logic;                      
    reset      : in  std_logic;                      
    i_valid    : in  std_logic;                 
    i_pixel    : in  unsigned(7 downto 0);
    o_valid    : out std_logic;                 
    o_edge     : out std_logic;	                     
    o_dir      : out std_logic_vector(2 downto 0);
    o_mode     : out mode_ty;
    o_row      : out unsigned(7 downto 0);
    o_col      : out unsigned(7 downto 0)
  );  
end kirsch;


architecture main of kirsch is
  signal state : state_ty := resetState;
  signal cycle : cal_state_ty := cycle_00;

  signal col_index  : unsigned(7 downto 0) := "00000000";
  signal row_index  : unsigned(7 downto 0) := "00000000";

  signal max0_val  : unsigned(9 downto 0);
  signal max0_cmp  : std_logic;

  signal max0_a    : unsigned(9 downto 0);
  signal max0_b    : unsigned(9 downto 0);

  signal max1_val  : unsigned(9 downto 0);
  signal max1_cmp  : std_logic;

  signal max1_a    : unsigned(9 downto 0);
  signal max1_b    : unsigned(9 downto 0);

  signal rdy_calc  : std_logic := '0';
  signal rdy_assign  : std_logic := '0';

  signal row_wr_en  : unsigned(2 downto 0) := "000";

  signal ra : std_logic_vector(7 downto 0);
  signal rb : std_logic_vector(7 downto 0);
  signal rc : std_logic_vector(7 downto 0);
  signal rd : std_logic_vector(7 downto 0);
  signal re : std_logic_vector(7 downto 0);
  signal rf : std_logic_vector(7 downto 0);
  signal rg : std_logic_vector(7 downto 0);
  signal rh : std_logic_vector(7 downto 0);

  signal r0 : std_logic_vector(7 downto 0);
  signal r1 : std_logic_vector(8 downto 0);
  signal r2 : std_logic_vector(9 downto 0);
  signal r3 : std_logic_vector(9 downto 0);
  signal r4 : std_logic_vector(11 downto 0);
  signal r_out : std_logic_vector(12 downto 0);

  signal row1_read : std_logic_vector(7 downto 0);
  signal row2_read : std_logic_vector(7 downto 0);
  signal row3_read : std_logic_vector(7 downto 0);

  signal o_cal: integer;
  signal cnt:  unsigned(7 downto 0) := "00000000";
begin
  row0: entity WORK.mem
    port map (
      address => col_index,
      clock   => clk,
      data    => std_logic_vector(i_pixel),
      wren    => row_wr_en(0),
      q       => row0_read
    );

  row1: entity WORK.mem
    port map (
      address => col_index,
      clock   => clk,
      data    => std_logic_vector(i_pixel),
      wren    => row_wr_en(1),
      q       => row1_read
    );

  row2: entity WORK.mem
    port map (
      address => col_index,
      clock   => clk,
      data    => std_logic_vector(i_pixel),
      wren    => row_wr_en(2),
      q       => row2_read
    );

  -- max0: entity WORK.max
  --   port map (
  --     o_val => max0_val,
  --     o_cmp => max0_cmp,
  --     i_a   => max0_a,
  --     i_b   => max0_b
  --   );

  -- max1: entity WORK.max
  -- port map (
  --   o_val => max1_val,
  --   o_cmp => max1_cmp,
  --   i_a   => max1_a,
  --   i_b   => max1_b
  -- );
    

process io
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
        o_mode <= o_busy;
        if (i_valid) then
          -- at the end of any row
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            col_index <= to_unsigned(0, 8);
            row_index <= row_index + 1;
          end if;

          -- finished filling up first 2 column on row 2
          if (col_index >= to_unsigned(2, 8) and row_index >= to_unsigned(2,8)) then
            rdy_calc <= 1;
          end if;
            --
          col_index <= col_index + 1;
          
            --update current input
          if (col_index = '0') then
            rg <= i_pixel;
          else if (col_index = '1') then
            rf <= i_pixel;
          else 
            -- reassign intermediates
            ra <= rb;
            rb <= rc;
            rh <= ri;
            ri <= rd;
            rg <= rf;
            rf <= re;
            re <= i_pixel;
          end if;

          case row_index is
            -- row0   a b c
            -- row1   h i d
            -- row2   g f e
            when 2 =>
            -- currently writing row 2
              if (col_index >=2) then
                rc <= row0_read;
                rd <= row1_read;
              else if (col_index = to_unsigned(0,1)) then
                rh <= row1_read;
                ra <= row0_read;
              else 
                ri <= row1_read;
                rb <= row0_read;
              end if;
            when 1 =>
            -- currently writing row 1
              if (col_index >=2) then
                rc <= row2_read;
                rd <= row0_read;
              else if (col_index = to_unsigned(0,1)) then
                rh <= row0_read;
                ra <= row2_read;
              else 
                ri <= row0_read;
                rb <= row2_read;
              end if;
            when 0 =>
            -- currently writing row 0
              if (col_index >=2) then
                rc <= row1_read;
                rd <= row2_read;
              else if (col_index = to_unsigned(0,1)) then
                ra <= row1_read;
                rh <= row2_read;
              else 
                rb <= row1_read;
                ri <= row2_read;
              end if;
        end case;
      when others =>

    end case;
  end if;
end process;

process calcDeriv
begin
wait until rising_edge(clk);
  if (reset = '1') then 
    -- prep for the cycle_00
    max0_a <= rb;
    max0_b <= rg;

    max1_a <= r3;
    max1_b <= r2;

    cycle <= cycle_00;
  end if

  if (rdy_calc) then
    r0 <= max0_val;
    r3 <= max1_val;

    r2 <= r0 + r1;
    r4 <= r2 + r4;

    case cycle is 
      when cycle_00 => 
        max0_a <= ra;
        max0_b <= rd;

        r1 <= ra + rh;
        r4 <= r2 + r4;

        cycle <= cycle_01;
      when cycle_01 => 
        max0_a <= rc;
        max0_b <= rf;

        r3 <= r2;

        r1 <= rb + rc;
        r4 <= r2 + r4;

        cycle <= cycle_02;
      when cycle_02 => 
        max0_a <= re;
        max0_b <= rh;

        r1 <= re + rd;
        r3 <= r2;
        r4 <= r2;

        r_out <= r3&'00' - r4&'0' - r4);
        cycle <= cycle_03;
      when cycle_03 => 
        max0_a <= rb;
        max0_b <= rg;

        r1 <= rf + rg;
        o_edge <= (r_out > to_unsigned(383, 12));
        cycle <= cycle_00;
    end case;
  end if;
end process;

end architecture main;
