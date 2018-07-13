library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.numeric_std_unsigned.all ;

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
  constant cycle_00 : cal_state_ty := "000";
  constant cycle_01 : cal_state_ty := "001";
  constant cycle_02 : cal_state_ty := "010";
  constant cycle_03 : cal_state_ty := "011";
  constant cycle_04 : cal_state_ty := "100";
  constant cycle_05 : cal_state_ty := "101";
  constant cycle_06 : cal_state_ty := "110";
  constant cycle_07 : cal_state_ty := "111";
end ostate_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;
use work.kirsch_synth_pkg.all;
use work.ostate_pkg.all;
use ieee.numeric_std_unsigned.all;

entity kirsch is
  port (
    clk        : in  std_logic;                      
    reset      : in  std_logic;                      
    i_valid    : in  std_logic;                 
    i_pixel    : in  unsigned(7 downto 0);
    o_valid    : out std_logic;                 
    o_edge     : out std_logic;	                     
    o_dir      : out std_logic_vector(2 downto 0);
    o_mode     : out work.ostate_pkg.mode_ty;
    o_row      : out unsigned(7 downto 0);
    o_col      : out unsigned(7 downto 0)
  );  
end kirsch;


architecture main of kirsch is
  signal state : state_ty := resetState;
  signal cycle : cal_state_ty := cycle_00;

  signal col_index  : unsigned(7 downto 0) := "00000000";
  signal row_index  : unsigned(7 downto 0) := "00000000";

  signal max0_val  : std_logic_vector(9 downto 0);
  signal max0_cmp  : std_logic;

  signal max0_a    : unsigned(9 downto 0);
  signal max0_b    : unsigned(9 downto 0);

  signal max1_val  : std_logic_vector(9 downto 0);
  signal max1_cmp  : std_logic;

  signal max1_a    : unsigned(9 downto 0);
  signal max1_b    : unsigned(9 downto 0);

  signal rdy_calc  : std_logic := '0';
  signal rdy_assign  : std_logic := '0';
  signal first_process  : std_logic := '1';

  signal row_wr_en  : unsigned(2 downto 0) := "000";

  signal ra : unsigned(7 downto 0) := "00000000";
  signal rb : unsigned(7 downto 0) := "00000000";
  signal rc : unsigned(7 downto 0) := "00000000";
  signal rd : unsigned(7 downto 0) := "00000000";
  signal re : unsigned(7 downto 0) := "00000000";
  signal rf : unsigned(7 downto 0) := "00000000";
  signal rg : unsigned(7 downto 0) := "00000000";
  signal rh : unsigned(7 downto 0) := "00000000";
  signal ri : unsigned(7 downto 0) := "00000000";

  signal m01_a : std_logic;
  signal m32_a : std_logic;
  signal m42_a : std_logic;
  signal m52_a : std_logic;
  signal m11_a : std_logic;
  signal m21_a : std_logic;
  signal m31_a : std_logic;

  signal m01_b : std_logic;
  signal m32_b : std_logic;
  signal m42_b : std_logic;
  signal m52_b : std_logic;
  signal m11_b : std_logic;
  signal m21_b : std_logic;
  signal m31_b : std_logic;

  signal r0 : unsigned(9 downto 0) := "0000000000";
  signal r1 : unsigned(9 downto 0) := "0000000000";
  signal r2 : unsigned(9 downto 0) := "0000000000";
  signal r3 : unsigned(9 downto 0) := "0000000000";
  signal r4 : unsigned(11 downto 0) := "000000000000";
  signal r_out : signed(12 downto 0) := "0000000000000";

  signal ri_row_a : unsigned(7 downto 0) := "00000000";
  signal ri_col_a : unsigned(7 downto 0) := "00000000";

  signal ri_row_b : unsigned(7 downto 0) := "00000000";
  signal ri_col_b : unsigned(7 downto 0) := "00000000";

  signal row0_read : std_logic_vector(7 downto 0);
  signal row1_read : std_logic_vector(7 downto 0);
  signal row2_read : std_logic_vector(7 downto 0);

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

  max0: entity WORK.max
    port map (
      o_val => max0_val,
      o_eqb => max0_cmp,
      i_clk => clk,
      i_a   => std_logic_vector(max0_a),
      i_b   => std_logic_vector(max0_b)
    );

  max1: entity WORK.max
  port map (
    o_val => max1_val,
    o_eqb => max1_cmp,
    i_clk => clk,
    i_a   => std_logic_vector(max1_a),
    i_b   => std_logic_vector(max1_b)
  );
    

process
begin
wait until rising_edge(clk);
  if (reset = '1') then
    -- this is in reset for both the mode and our internal statemachine
    state   <= resetState;
    o_mode  <= o_reset;

    col_index <= to_unsigned(0, 8);
    row_index <= to_unsigned(0, 8);

    row_wr_en <= to_unsigned(1, 3);
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
        if (i_valid = '1') then
          -- at the end of any row
          col_index <= col_index + 1;
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            col_index <= to_unsigned(0, 8);
            row_index <= row_index + 1;
          end if;

          -- finished filling up first 2 column on row 2
          -- condition tripped on first run
          if (col_index = 2 and row_index = 2) then
            rdy_calc <= '1';
          end if;

            --update current input
          if (col_index = 0) then
            rg <= unsigned(i_pixel);
          elsif (col_index = 1) then
            rf <= unsigned(i_pixel);
          elsif (col_index = 2) then
            re <= unsigned(i_pixel);
            ri_col_a <= col_index-1;
            ri_row_a <= row_index-1;
          else 
            -- reassign intermediates
            ra <= rb;
            rb <= rc;
            rh <= ri;
            ri <= rd;
            rg <= rf;
            rf <= re;
            ri_col_a <= col_index-1;
            ri_row_a <= row_index-1;
            re <= unsigned(i_pixel);
          end if;

          case to_integer(unsigned(row_wr_en)) is
            -- row0   a b c
            -- row1   h i d
            -- row2   g f e
            when 4 =>
            -- currently writing row 2
            -- TODO find out assignment for row and col index
              if (col_index >= 2) then
                rc <= unsigned(row0_read);
                rd <= unsigned(row1_read);
              elsif (col_index = 1) then
                ri <= unsigned(row1_read);
                rb <= unsigned(row0_read);
              else 
                rh <= unsigned(row1_read);
                ra <= unsigned(row0_read);
              end if;
            when 2 =>
            -- currently writing row 1
            -- TODO find out assignment for row and col index
              if (col_index >= 2) then
                rc <= unsigned(row2_read);
                rd <= unsigned(row0_read);
              elsif (col_index = 1) then
                ri <= unsigned(row0_read);
                rb <= unsigned(row2_read);
              else 
                rh <= unsigned(row0_read);
                ra <= unsigned(row2_read);
              end if;
            when 1 =>
            -- currently writing row 0
            -- TODO find out assignment for row and col index
              if (col_index >= 2) then
                rc <= unsigned(row1_read);
                rd <= unsigned(row2_read);
              elsif (col_index = 1) then
                rb <= unsigned(row1_read);
                ri <= unsigned(row2_read);
              else 
                ra <= unsigned(row1_read);
                rh <= unsigned(row2_read);
              end if;
            when others =>
                    null;
          end case;
        end if;
      when others =>
        null;
    end case;
  end if;
end process;

process
begin
wait until rising_edge(clk);
  if (reset = '1') then 
    max0_a <= "00"&rg;
    max0_b <= "00"&rb;

    max1_a <=  r3;
    max1_b <=  r2;
  elsif (rdy_calc) then
    r0 <= unsigned(max0_val);
    r3 <= unsigned(max1_val);

    max1_a <= r3;
    max1_b <= r2;

    r2 <= r0 + r1;
    r4 <= r2 + r4;

    o_valid <= '0';

    case cycle is 
      when cycle_00 => 
        ri_col_b <= ri_col_a;
        ri_row_b <= ri_row_a;
        
        cycle <= cycle_01;
        m01_a <= max0_cmp;
        m42_b <= max1_cmp;

        max0_a <= "00"&ra;
        max0_b <= "00"&rd;

        r1(7 downto 0) <= ra + rh;
        r4 <= r2 + r4;
        
      when cycle_01 => 
        cycle <= cycle_02;
        m11_a <= max0_cmp;
        m52_b <= max1_cmp;

        max0_a <= "00"&rc;
        max0_b <= "00"&rf;
        r3 <= r2;
        r1(7 downto 0) <= rb + rc;
        r4 <= r2 + r4;

      when cycle_02 => 
        cycle <= cycle_03;
        m21_a <= max0_cmp;

        max0_a <= "00"&re;
        max0_b <= "00"&rh;
        r1(7 downto 0) <= re + rd;
        r3 <= r2;
        r4 <= "00"&r2;

        o_dir(2) <= (m01_b and not m32_b and not m42_b and not m52_b) or 
                    (m11_b and m32_b and not m42_b and not m52_b) or
                    (m21_b and m42_b and not m52_b) or
                    (m31_b and m52_b);

        r_out <= (signed(r3&"000") - signed(r4&'0') - signed(r4));

      when cycle_03 => 
        if (i_valid = '1') then
          cycle <= cycle_04;
          max0_a <= "00"&rb;
          max0_b <= "00"&rg;
        end if;

        o_dir(1) <= (m32_b and not m42_b and not m52_b) or m52_b;
        o_dir(0) <= (not m01_b and not m32_b and not m42_b and not m52_b) or
                    (m21_b and m42_b and not m52_b) or m52_b;

        r1(7 downto 0) <= rf + rg;
        m31_a <= max0_cmp;
        m32_a <= max1_cmp;

        if (first_process = '1')  then
          first_process <= '0';
        else 
          o_valid <= '1';
          if (r_out > to_signed(383, 13)) then
            o_edge <= '1';
          else 
            o_edge <= '0';
            o_dir <= "000";
          end if;

          o_row  <= ri_row_b;
          o_col  <= ri_col_b;        

          -- o_dir  <= 
        end if;

      when cycle_04 => 
        ri_col_b <= ri_col_a;
        ri_row_b <= ri_row_a;

        cycle <= cycle_05;

        m42_a <= max1_cmp;
        m01_b <= max0_cmp;
        
        max0_a <= "00"&ra;
        max0_b <= "00"&rd;

        r1(7 downto 0) <= ra + rh;
        r4 <= r2 + r4;

      when cycle_05 =>
        cycle <= cycle_06;
        m52_a <= max1_cmp;
        m11_b <= max0_cmp;

        max0_a <= "00"&rc;
        max0_b <= "00"&rf;
        r3 <= r2;
        r1(7 downto 0) <= rb + rc;
        r4 <= r2 + r4;

      when cycle_06 => 
        cycle <= cycle_07;

        m21_b <= max0_cmp;

        max0_a <= "00"&re;
        max0_b <= "00"&rh;
        r1(7 downto 0) <= re + rd;
        r3 <= r2;
        r4 <= "00"&r2;

        o_dir(2) <= (m01_a and not m32_a and not m42_a and not m52_a) or 
                    (m11_a and m32_a and not m42_a and not m52_a) or
                    (m21_a and m42_a and not m52_a) or
                    (m31_a and m52_a);
        r_out <= (signed(r3&"000") - signed(r4&'0') - signed(r4));

      when cycle_07 => 
        cycle <= cycle_00;
        if (i_valid = '1') then
          cycle <= cycle_07;
          max0_a <= "00"&rg;
          max0_b <= "00"&rb;
        end if;

        r1(7 downto 0) <= rf + rg;

        m31_b <= max0_cmp;
        m32_b <= max1_cmp;
        o_dir(1) <= (m32_a and not m42_a and not m52_a) or m52_a;
        o_dir(0) <= (not m01_a and not m32_a and not m42_a and not m52_a) or
                    (m21_a and m42_a and not m52_a) or m52_a;

        o_valid <= '1';
        if (r_out > to_signed(383, 13)) then
          o_edge <= '1';
        else 
          o_edge <= '0';
          o_dir <= "000";
        end if;

        o_row  <= ri_row_b;
        o_col  <= ri_col_b;
        -- o_dir  <= 

      when others =>
        null;

    end case;
  end if;
end process;

end architecture main;
