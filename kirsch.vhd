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
  constant state_00 : state_ty := "00"; -- cycle_00 and resetState
  constant state_01 : state_ty := "01"; -- cycle_01 and firstFill
  constant state_02 : state_ty := "10"; -- cycle_02 and fetchPixel
  constant state_03 : state_ty := "11"; -- cycle_03 and result
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
  signal row_wr_en : unsigned(2 downto 0);

  -- intermediate signal
  signal state, cycle : state_ty;

  signal col_index, row_index  : unsigned(7 downto 0);

  signal rdy_calc      : std_logic := '0';
  signal first_process : std_logic := '1';

  signal ra, rb, rc, rd, re, rf, rg, rh, ri : unsigned(7 downto 0);

  signal is_a : std_logic;

  signal m01_a, m01_b : std_logic;
  signal m32_a, m32_b : std_logic;
  signal m42_a, m42_b : std_logic;
  signal m52_a, m52_b : std_logic;
  signal m11_a, m11_b : std_logic;
  signal m21_a, m21_b : std_logic;
  signal m31_a, m31_b : std_logic;

  signal r0 : unsigned(7 downto 0) := "00000000";
  signal r1 : unsigned(8 downto 0) := "000000000";
  signal r2 : unsigned(9 downto 0) := "0000000000";
  signal r3 : unsigned(9 downto 0) := "0000000000";
  signal r4_a : unsigned(12 downto 0):= "0000000000000";
  signal r4_b : unsigned(12 downto 0):= "0000000000000";
  
  signal r_out: signed(14 downto 0):= "000000000000000";

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
  if (reset) then 
    -- this is in reset for both the mode and our internal statemachine
    state   <= state_00;
    o_mode  <= o_reset;

    col_index <= to_unsigned(0, 8);
    row_index <= to_unsigned(0, 8);
    row_wr_en <= to_unsigned(1, 3);

    ra <= to_unsigned(0, 8);
    rb <= to_unsigned(0, 8);
    rc <= to_unsigned(0, 8);
    rd <= to_unsigned(0, 8);
    re <= to_unsigned(0, 8);
    rf <= to_unsigned(0, 8);
    rg <= to_unsigned(0, 8);
    rh <= to_unsigned(0, 8);
    ri <= to_unsigned(0, 8);
  else
    case state is
      when state_00 =>
        o_mode <= o_idle;
        if (i_valid = '1') then 
          state <= state_01;
          col_index <= col_index + 1;
        end if;

      -- need to fill up to at least the first 2 row
      when state_01 =>
        o_mode <= o_busy;
        rdy_calc <= '0';

        if (i_valid = '1') then
          col_index <= col_index + 1;

          -- at the end of row
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            row_index <= row_index + 1;

            if (row_index = to_unsigned(1, 8)) then
              state <= state_02;
            end if;
          end if;
        end if;
      
      -- wait for this to fill the column 0 and then start calculating
      when others =>
        -- finished filling up column 0
        if (i_valid = '1') then 
          col_index <= col_index + 1;
          rdy_calc <= '1';

          -- at the end of row
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
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
            ri_col_a <= col_index-2;
            ri_row_a <= row_index-1;
            re <= unsigned(i_pixel);
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
    end case;
  end if;
end process;

process
  begin
  wait until rising_edge(clk);
  if (reset) then
    o_edge <= '0';
    o_valid <= '0';
    o_dir <= "000";

    cycle <= state_00;
    is_a <= '1';

    r1 <= to_unsigned(0,9);
    r2 <= to_unsigned(0,10);
    r3 <= to_unsigned(0,10);
    r4_a <= to_unsigned(0,13);
    r4_b <= to_unsigned(0,13);

    m01_a <= '0';
    m01_b <= '0';
    m32_a <= '0';
    m32_b <= '0';
    m42_a <= '0';
    m42_b <= '0';
    m52_a <= '0';
    m52_b <= '0';
    m11_a <= '0';
    m11_b <= '0';
    m21_a <= '0';
    m21_b <= '0';
    m31_a <= '0';
    m31_b <= '0';

  else 
    if (rdy_calc) then
      if (i_valid) then
        r2 <= "00"&r0 + r1;
        r4_a <= "0000"&r1 + r4_a;
        r4_b <= "0000"&r1 + r4_b;
      end if;

      case cycle is 
        when state_00 => 
          cycle <= state_01;

          r1 <= "0"&ra + rh;

          if (is_a) then 
            r4_a <= (others => '0');
            m01_a <= '1' when rg < rb else '0';
            m42_b <= '1' when r3 < r2 else '0';
          else 
            r4_b <= (others => '0');
            m01_b <= '1' when rg < rb else '0';
            m42_a <= '1' when r3 < r2 else '0';
          end if;

          if rg >= rb then 
            r0 <= rg;
          else 
            r0 <= rb;
          end if;

          if r3 < r2 then 
            r3 <= r2;
          end if;
        
        when state_01 => 
          cycle <= state_02;

          ri_col_b <= ri_col_a;
          ri_row_b <= ri_row_a;

          r1 <= "0"&rb + rc;

          if (is_a) then 
            m11_a <= '1' when ra < rd else '0';
            m52_b <= '1' when r3 < r2 else '0';
            r_out <= signed(unsigned(r4_b&"00")) - signed(unsigned(r4_b));
          else 
            m11_b <= '1' when ra < rd else '0';
            m52_a <= '1' when r3 < r2 else '0';
            r_out <= signed(unsigned(r4_a&"00")) - signed(unsigned(r4_a));
          end if;

          if ra >= rd then 
            r0 <= ra;
          else 
            r0 <= rd;
          end if;

          if r3 < r2 then 
            r3 <= r2;
          end if;

        when state_02 => 
          cycle <= state_03;

          o_row  <= ri_row_b;
          o_col  <= ri_col_b;

          r3 <= r2;
          r1 <= "0"&re + rd;

          if (is_a) then 
            m21_a <= '1' when rc < rf else '0';
          else 
            m21_b <= '1' when rc < rf else '0';
          end if;

          if rc >= rf then
            r0 <= rc;
          else 
            r0 <= rf;
          end if;
          
          if (first_process)  then
            first_process <= '0';
          else 
            o_valid <= '1';
            if ((signed(unsigned("0"&r3&"000")) - r_out) > to_signed(383, 13)) then
              o_edge <= '1';
              if (is_a) then
                o_dir(2) <= (m01_b and not m32_b and not m42_b and not m52_b) or 
                            (m11_b and m32_b and not m42_b and not m52_b) or
                            (m21_b and m42_b and not m52_b) or
                            (m31_b and m52_b);
                o_dir(1) <= (m32_b and not m42_b and not m52_b) or m52_b;
                o_dir(0) <= (not m01_b and not m32_b and not m42_b and not m52_b) or
                            (m21_b and m42_b and not m52_b) or m52_b;
              else 
                o_dir(2) <= (m01_a and not m32_a and not m42_a and not m52_a) or 
                            (m11_a and m32_a and not m42_a and not m52_a) or
                            (m21_a and m42_a and not m52_a) or
                            (m31_a and m52_a);
                o_dir(1) <= (m32_a and not m42_a and not m52_a) or m52_a;
                o_dir(0) <= (not m01_a and not m32_a and not m42_a and not m52_a) or
                            (m21_a and m42_a and not m52_a) or m52_a;
              end if;
            else 
              o_edge <= '0';
              o_dir <= "000";
            end if;    
          end if;

        when others => 
          r1 <= "0"&rf + rg;
          
          o_edge <= '0';
          o_dir <= "000";
          o_valid <= '0';

          if (i_valid) then
            cycle <= state_00;
          end if;

          if (is_a) then 
            m31_a <= '1' when re < rh else '0';
            m32_a <= '1' when r3 < r2 else '0';
            r4_b <= (others => '0');
          else 
            m31_b <= '1' when re < rh else '0';
            m32_b <= '1' when r3 < r2 else '0';
            r4_a <= (others => '0');
          end if;
            
          if re >= rh then 
            r0 <= re;
          else 
            r0 <= rh;
          end if;

          if r3 < r2 then 
            r3 <= r2;
          end if;

          is_a <= not is_a;
      end case;
    end if;
  end if;
  end process;
end architecture main;