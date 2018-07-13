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
  constant resetState : state_ty := "00";
  constant firstFill  : state_ty := "01";
  constant fetchPixel : state_ty := "10";
  constant result     : state_ty := "11";

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
  -- mem enetity port signal
  signal row0_read, row1_read, row2_read : std_logic_vector(7 downto 0);
  signal row_wr_en : unsigned(2 downto 0) := "100";

  -- intermediate signal
  signal state : state_ty := resetState;
  signal cycle : cal_state_ty := cycle_00;

  signal col_index, row_index  : unsigned(7 downto 0) := "00000000";

  signal rdy_calc      : std_logic := '0';
  signal rdy_assign    : std_logic := '0';
  signal first_process : std_logic := '1';

  signal ra, rb, rc, rd, re, rf, rg, rh, ri : unsigned(7 downto 0) := "00000000";

  signal m01_a, m01_b : std_logic;
  signal m32_a, m32_b : std_logic;
  signal m42_a, m42_b : std_logic;
  signal m52_a, m52_b : std_logic;
  signal m11_a, m11_b : std_logic;
  signal m21_a, m21_b : std_logic;
  signal m31_a, m31_b : std_logic;

  signal r0 : unsigned(7 downto 0);
  signal r1 : unsigned(8 downto 0);
  signal r2 : unsigned(9 downto 0);
  signal r3 : unsigned(9 downto 0);
  signal r4 : unsigned(12 downto 0);

  signal r_out : signed(12 downto 0);

  signal ri_row_a, ri_row_b : unsigned(7 downto 0) := "00000000";
  signal ri_col_a, ri_col_b : unsigned(7 downto 0) := "00000000";
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
        o_mode <= o_idle;
        if (i_valid = '1') then 
          state <= firstFill;
          col_index <= col_index + 1;
        end if;

      -- need to fill up to at least the first 2 row
      when firstFill =>
        o_mode <= o_busy;
        rdy_calc <= '0';

        if (i_valid = '1') then
          col_index <= col_index + 1;

          -- at the end of row
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            col_index <= to_unsigned(0, 8);
            row_index <= row_index + to_unsigned(1, 8);

            if (row_index = to_unsigned(1, 8)) then
              state <= fetchPixel;
            end if;
          end if;
        end if;
      
      -- wait for this to fill the column 0 and then start calculating
      when fetchPixel =>
        -- finished filling up column 0
        if (i_valid = '1') then 
          col_index <= col_index + 1;
          rdy_calc <= '1';

          -- at the end of row
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            col_index <= to_unsigned(0, 8);
            row_index <= row_index + 1;
          end if;

          -- if we are filling column 0/1, do not start calculation
          if (col_index = to_unsigned(0, 8)) then
            rg <= unsigned(i_pixel);

            rdy_calc <= '0';
          elsif (col_index = to_unsigned(1, 8)) then
            rf <= unsigned(i_pixel);
            rdy_calc <= '0';
          elsif (col_index = to_unsigned(2, 8)) then 
            re <= unsigned(i_pixel);
          else
            ra <= rb;
            rb <= rc;
            rh <= ri;
            ri <= rd;
            rg <= rf;
            rf <= re;
            ri_col_a <= col_index-1;
            ri_row_a <= row_index-1;
          end if;

          case row_wr_en is
            when to_unsigned(4, 3) =>
              -- currently writing row 2
              -- row0   a b c
              -- row1   h i d
              -- row2   g f e
              if (col_index = to_unsigned(0, 8)) then
                rh <= unsigned(row1_read);
                ra <= unsigned(row0_read);
              elsif (col_index = to_unsigned(1, 8)) then
                ri <= unsigned(row1_read);
                rb <= unsigned(row0_read);
              else 
                rc <= unsigned(row0_read);
                rd <= unsigned(row1_read);
              end if;

            when to_unsigned(2, 3) =>
              -- currently writing row 1
              -- row0   h i d
              -- row1   g f e
              -- row2   a b c
              if (col_index = to_unsigned(0, 8)) then
                rh <= unsigned(row0_read);
                ra <= unsigned(row2_read);
              elsif (col_index = to_unsigned(1, 8)) then
                ri <= unsigned(row0_read);
                rb <= unsigned(row2_read);
              else 
                rc <= unsigned(row2_read);
                rd <= unsigned(row0_read);
              end if;

            when others => -- when 1 =>
              -- currently writing row 0
              -- row0   g f e
              -- row1   a b c
              -- row2   h i d
              if (col_index = to_unsigned(0, 8)) then
                ra <= unsigned(row1_read);
                rh <= unsigned(row2_read);
              elsif (col_index = to_unsigned(1, 8)) then
                rb <= unsigned(row1_read);
                ri <= unsigned(row2_read);
              else 
                rc <= unsigned(row1_read);
                rd <= unsigned(row2_read);
              end if;
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
  if (rdy_calc) then
    r2 <= "000"&r0 + r1;
    r4 <= r4 + r2;

    o_valid <= '0';

    case cycle is 
      when cycle_00 => 
        cycle <= cycle_01;

        if rg > rb then 
          r0 <= rg;
          m01_a <= '0';
        else 
          r0 <= rb;
          m01_a <= '1';
        end if;

        if r3 > r2 then 
          r3 <= r3;
          m42_b <= '0';
        else 
          r3 <= r2;
          m42_b <= '1';
        end if;

        -- m01_a <= max0_cmp;
        -- m42_b <= max1_cmp;
        -- max0_a <= ra;
        -- max0_b <= rd;

        r1 <= "0"&ra + rh;
        
      when cycle_01 => 
        cycle <= cycle_02;

        ri_col_b <= ri_col_a;
        ri_row_b <= ri_row_a;

        -- m11_a <= max0_cmp;
        -- m52_b <= max1_cmp;
        -- max0_a <= rc;
        -- max0_b <= rf;

        if ra > rd then 
          r0 <= ra;
          m11_a <= '0';
        else 
          r0 <= rd;
          m11_a <= '1';
        end if;

        if r3 > r2 then 
          r3 <= r3;
          m52_b <= '0';
        else 
          r3 <= r2;
          m52_b <= '1';
        end if;

        r1 <= "0"&rb + rc;

      when cycle_02 => 
        cycle <= cycle_03;
        -- m21_a <= max0_cmp;
        -- max0_a <= re;
        -- max0_b <= rh;

        if rc > rf then
          r0 <= rc;
          m21_a <= '0';
        else 
          r0 <= rf;
          m21_a <= '1';
        end if;

        r3 <= r2;
        
        r1 <= "0"&re + rd;
        r4 <= "000"&r2;

        o_dir(2) <= (m01_b and not m32_b and not m42_b and not m52_b) or 
                    (m11_b and m32_b and not m42_b and not m52_b) or
                    (m21_b and m42_b and not m52_b) or
                    (m31_b and m52_b);

        r_out <= (signed(shift_left(r3, 3)) - signed(shift_left(r4, 1)) - signed(r4));

      when cycle_03 => 
        if (i_valid) then
          cycle <= cycle_04;
        end if;

        -- max0_a <= rb;
        -- max0_b <= rg;
        -- m31_a <= max0_cmp;
        -- m32_a <= max1_cmp;

        if re > rh then 
          r0 <= re;
          m31_a <= '0';
        else 
          r0 <= rh;
          m31_a <= '1';
        end if;

        if r3 > r2 then 
          r3 <= r3;
          m32_a <= '0';
        else 
          r3 <= r2;
          m32_a <= '1';
        end if;

        r1 <= "0"&rf + rg;

        -- output signal
        o_dir(1) <= (m32_b and not m42_b and not m52_b) or m52_b;
        o_dir(0) <= (not m01_b and not m32_b and not m42_b and not m52_b) or
                    (m21_b and m42_b and not m52_b) or m52_b;

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
        end if;

      when cycle_04 => 
        cycle <= cycle_05;

        if rg > rb then 
          r0 <= rb;
          m01_b <= '0';
        else 
          r0 <= rg;
          m01_b <= '1';
        end if;

        if r3 > r2 then 
          r3 <= r3;
          m42_a <= '0';
        else 
          r3 <= r2;
          m42_a <= '1';
        end if;

        r1 <= "0"&ra + rh;

        -- m42_a <= max1_cmp;
        -- m01_b <= max0_cmp;
        
        -- max0_a <= ra;
        -- max0_b <= rd;

      when cycle_05 =>
        cycle <= cycle_06;

        ri_col_b <= ri_col_a;
        ri_row_b <= ri_row_a;

        -- m52_a <= max1_cmp;
        -- m11_b <= max0_cmp;
        -- max0_a <= rc;
        -- max0_b <= rf;

        if ra > rd then 
          r0 <= ra;
          m11_b <= '0';
        else 
          r0 <= rd;
          m11_b <= '1';
        end if;

        if r3 > r2 then 
          r3 <= r3;
          m52_a <= '0';
        else 
          r3 <= r2;
          m52_a <= '1';
        end if;

        r1 <= "0"&rb + rc;

      when cycle_06 => 
        cycle <= cycle_07;
        -- m21_b <= max0_cmp;
        -- max0_a <= re;
        -- max0_b <= rh;

        if rc > rf then 
          r0 <= rc;
          m21_b <= '0';
        else 
          r0 <= rf;
          m21_b <= '1';
        end if;
        r3 <= r2;

        r1 <= "0"&re + rd;
        r4 <= "000"&r2;

        o_dir(2) <= (m01_a and not m32_a and not m42_a and not m52_a) or 
                    (m11_a and m32_a and not m42_a and not m52_a) or
                    (m21_a and m42_a and not m52_a) or
                    (m31_a and m52_a);

        r_out <= (signed(shift_left(r3, 3)) - signed(shift_left(r4, 1)) - signed(r4));

      when cycle_07 => 
        if (i_valid) then
          cycle <= cycle_00;
        end if;

        -- max0_a <= rg;
        -- max0_b <= rb;
        -- m31_b <= max0_cmp;
        -- m32_b <= max1_cmp;

        if re > rh then 
          r0 <= re;
          m31_b <= '0';
        else 
          r0 <= rh;
          m31_b <= '1';
        end if;

        if r3 > r2 then 
          r3 <= r3;
          m32_b <= '0';
        else 
          r3 <= r2;
          m32_b <= '1';
        end if;

        r1 <= "0"&rf + rg;

        o_dir(1) <= (m32_a and not m42_a and not m52_a) or m52_a;
        o_dir(0) <= (not m01_a and not m32_a and not m42_a and not m52_a) or
                    (m21_a and m42_a and not m52_a) or m52_a;

        o_valid <= not first_process;
        if (r_out > to_signed(383, 13)) then
          o_edge <= '1';
        else 
          o_edge <= '0';
          o_dir <= "000";
        end if;

        o_row  <= ri_row_b;
        o_col  <= ri_col_b;

      when others =>
        null;

    end case;
  end if;
end process;

end architecture main;
