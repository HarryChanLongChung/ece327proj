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

  signal rdy_calc, first_process, is_a, last_pixel : std_logic;

  signal ra, rb, rc, rd, re, rf, rg, rh, ri : unsigned(7 downto 0);

  signal da : std_logic_vector(2 downto 0); 
  signal db : std_logic_vector(2 downto 0);
  signal dc : std_logic_vector(2 downto 0);
  signal dd : std_logic_vector(2 downto 0);
  signal de : std_logic_vector(2 downto 0);

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

process (clk, reset)
begin
  if (reset = '1') then
    -- this is in reset for both the mode and our internal statemachine
    state   <= state_00;
    o_mode  <= o_reset;
    last_pixel <= '0';

    row_index <= to_unsigned(0, 8);

    row_wr_en <= to_unsigned(1, 3);
  elsif (clk'EVENT and clk='1') then 
    case state is
      when state_00 =>
        o_mode <= o_idle;
        if (i_valid = '1') then 
          state <= state_01;
          col_index <= to_unsigned(1,8);
          o_col <= to_unsigned(1,8);
        end if;

      -- need to fill up to at least the first 2 row
      when state_01 =>
        o_mode <= o_busy;
        rdy_calc <= '0';

        if (i_valid = '1') then
          col_index <= col_index + 1;
          o_col <= col_index + to_unsigned(1, 8);

          -- at the end of row
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            row_index <= row_index + to_unsigned(1, 8);
            o_row <= row_index + to_unsigned(1, 8);

            if (row_index = to_unsigned(1, 8)) then
              state <= state_02;
            end if;
          end if;
        end if;
      
      -- wait for this to fill the column 0 and then start calculating
      when state_02 =>
        -- finished filling up column 0
        if (i_valid = '1') then 
          col_index <= col_index + to_unsigned(1, 8);
          o_col <= col_index + to_unsigned(1, 8);
          rdy_calc <= '1';

          -- at the end of row
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            row_index <= row_index + to_unsigned(1, 8);
            o_row <= row_index + to_unsigned(1, 8);
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
            re <= unsigned(i_pixel);
            if (col_index = to_unsigned(255,8) and row_index = to_unsigned(255,8)) then
              last_pixel <= '1';
              state <= state_03;
            end if;
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
          if (cycle = state_03) then
            o_mode <= o_idle;
            state <= state_00;

            last_pixel <= '0';
            rdy_calc <= '0';
          end if;
    end case;
  end if;
end process;

process (clk, reset)
  begin
  if (reset) then
    cycle <= state_00;

    is_a <= '1';
    first_process <= '1';

    r4_a <= to_unsigned(0,13);
    r4_b <= to_unsigned(0,13);
  elsif (clk'EVENT and clk='1') then 
    if (rdy_calc) then
      o_valid <= '0';
      o_edge <= '0';
      o_dir <= "000";

      case cycle is 
        when state_00 => --0,4
          cycle <= state_01;

          r0 <= rb when rg < rb else rg;
          r1 <= "0"&ra + rh;
          r2 <= "00"&r0 + r1;

          r3 <= r2 when r3 < r2 else r3;

          de <= dc when r3 < r2 else de;
          da <= "100" when rg < rb else "001";

          if (is_a) then 
            r4_a <= (others => '0');
            r4_b <= "0000"&r1 + r4_b;
          else 
            r4_a <= "0000"&r1 + r4_a;
            r4_b <= (others => '0');
          end if;
        
        when state_01 => --1,5
          cycle <= state_02;

          r0 <= rd when ra < rd else ra;
          r1 <= "0"&rb + rc;
          r2 <= "00"&r0 + r1;

          r3 <= r2 when r3 < r2 else r3;

          db <= "110" when ra < rd else "010";
          de <= dd when r3 < r2 else de;

          if (is_a) then 
            r4_b <= r4_b;
            r4_a <= "0000"&r1 + r4_a;
            r_out <= signed(unsigned(r4_b&"00")) - signed(unsigned(r4_b));
          else 
            r4_a <= r4_a;
            r4_b <= "0000"&r1 + r4_b;
            r_out <= signed(unsigned(r4_a&"00")) - signed(unsigned(r4_a));
          end if;

        when state_02 => --2,6
          cycle <= state_03;

          r0 <= rf when rc < rf else rc;
          r1 <= "0"&re + rd;
          r2 <= "00"&r0 + r1;
          
          r3 <= r2;
                    
          de <= da;
          dc <= "101" when rc < rf else "000";

          if (is_a) then 
            r4_a <= "0000"&r1 + r4_a;
          else 
            r4_b <= "0000"&r1 + r4_b;
          end if;
          
          if (first_process)  then
            first_process <= '0';
          else 
            o_valid <= '1';
            if ((signed(unsigned("0"&r3&"000")) - r_out) > to_signed(383, 13)) then
              o_edge <= '1';
              o_dir <= de;
            else 
              o_edge <= '0';
              o_dir <= "000";
            end if;    
          end if;

        when others => --3,7
          r0 <= rh when re < rh else re;
          r1 <= "0"&rf + rg;
          r2 <= "00"&r0 + r1;
          r3 <= r2 when r3 < r2 else r3;

          dd <= "111" when re < rh else "011";
          de <= db when r3 < r2 else de;
          
          if (i_valid or last_pixel) then
            cycle <= state_00;
          end if;

          if (is_a) then 
            r4_a <= "0000"&r1 + r4_a;
            r4_b <= (others => '0');
          else 
            r4_a <= (others => '0');
            r4_b <= "0000"&r1 + r4_b;
          end if;

          is_a <= not is_a;
      end case;
    end if;
  end if;
  end process;
end architecture main;