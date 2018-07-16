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
  signal first_process : std_logic := '1';

  signal ra, rb, rc, rd, re, rf, rg, rh, ri : unsigned(7 downto 0) := "00000000";

  signal last_pixel : std_logic;

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

  signal r_out : signed(14 downto 0):= "000000000000000";

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
    last_pixel <= '0';

    col_index <= to_unsigned(0, 8);
    row_index <= to_unsigned(0, 8);

    row_wr_en <= to_unsigned(1, 3);

    ra <= to_unsigned(0,8);
    rb <= to_unsigned(0,8);
    rc <= to_unsigned(0,8);
    rd <= to_unsigned(0,8);
    re <= to_unsigned(0,8);
    rf <= to_unsigned(0,8);
    rg <= to_unsigned(0,8);
    rh <= to_unsigned(0,8);
    ri <= to_unsigned(0,8);

    rdy_calc <= '0';

    o_row <= to_unsigned(0,8);
    o_col <= to_unsigned(0,8);

  else
    case state is
      when resetState =>
        o_mode <= o_idle;
        rdy_calc <= '0';
        if (i_valid = '1') then 
          state <= firstFill;
          col_index <= to_unsigned(1,8);
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
            row_index <= row_index + to_unsigned(1, 8);

            if (row_index = to_unsigned(1, 8)) then
              state <= fetchPixel;
            end if;
          end if;
        end if;

        o_col <= col_index;
        o_row <= row_index;
      
      -- wait for this to fill the column 0 and then start calculating
      when fetchPixel =>
        -- finished filling up column 0
        if (i_valid = '1') then 
          col_index <= col_index + 1;
          rdy_calc <= '1';

          -- at the end of row
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            row_index <= row_index + 1;
          end if;
          o_col <= col_index;
          o_row <= row_index;
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
              state <= result;
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
          if (cycle = cycle_03) then
            last_pixel <= '0';
            o_mode <= o_idle;
            state <= resetState;
            rdy_calc <= '0';
          end if;
    end case;
  end if;
end process;

process
begin
wait until rising_edge(clk);
  if (reset = '1') then 
    da <= "000"; 
    db <= "000";
    dc <= "000";
    dd <= "000";
    de <= "000";

    r0 <= to_unsigned(0,8);
    r1 <= to_unsigned(0,9);
    r2 <= to_unsigned(0,10);
    r3 <= to_unsigned(0,10);
    r4_a <= to_unsigned(0,13);
    r4_b <= to_unsigned(0,13);

  else 

  if (rdy_calc) then
    o_valid <= '0';
    o_edge <= '0';
    o_dir <= "000";

    case cycle is 
      when cycle_00 => 
        cycle <= cycle_01;
        if rg >= rb then 
          r0 <= rg;
          da <= "001"; --W
        else 
          r0 <= rb;
          da <= "100"; --NW
        end if;

        if r3 < r2 then 
          r3 <= r2;
          de <= dc;
        end if;

        r1 <= "0"&ra + rh;
        r2 <= "00"&r0 + r1;
        r4_a <= (others => '0');
        r4_b <= "0000"&r1 + r4_b;
        
      when cycle_01 => 
        cycle <= cycle_02;

        if ra >= rd then 
          r0 <= ra;
          db <= "010"; --N
        else 
          r0 <= rd;
          db <= "110"; --NE
        end if;

        if r3 < r2 then 
          r3 <= r2;
          de <= dd;
        end if;

        r1 <= "0"&rb + rc;
        r2 <= "00"&r0 + r1;
        r4_b <= r4_b;
        r4_a <= "0000"&r1 + r4_a;

        r_out <= signed(unsigned(r4_b&"00")) - signed(unsigned(r4_b));
        
      when cycle_02 => 
        cycle <= cycle_03;

        if rc >= rf then
          r0 <= rc;
          dc <= "000"; --E
        else 
          r0 <= rf;
          dc <= "101"; --SE
        end if;

        r3 <= r2;
        de <= da;
        
        r1 <= "0"&re + rd;
        r2 <= "00"&r0 + r1;
        r4_a <= "0000"&r1 + r4_a;

        -- output signal for "red" set
        if (first_process = '1')  then
          first_process <= '0';
        else 
          o_valid <= '1';
          if signed(unsigned("0"&r3&"000")) - r_out > to_signed(383, 13) then
            o_edge <= '1';
            o_dir <= de;
          else 
            o_edge <= '0';
            o_dir <= "000";
          end if;
       
        end if;

      when cycle_03 => 
        if (i_valid) then
          cycle <= cycle_04;

        if re >= rh then 
          r0 <= re;
          dd <= "011"; --S
        else 
          r0 <= rh;
          dd <= "111"; --SW
        end if;

        if r3 < r2 then 
          r3 <= r2;
          de <= db;
        end if;

        r1 <= "0"&rf + rg;
        r2 <= "00"&r0 + r1;
        r4_b <= (others => '0');
        r4_a <= "0000"&r1 + r4_a;
        end if;
        if (last_pixel) then
          cycle <= cycle_00;
        end if;

      when cycle_04 => 
        cycle <= cycle_05;

        if rg >= rb then 
          r0 <= rg;
          da <= "001"; --W
        else 
          r0 <= rb;
          da <= "100"; --NW
        end if;

        if r3 < r2 then 
          r3 <= r2;
          de <= dc;
        end if;

        r1 <= "0"&ra + rh;
        r2 <= "00"&r0 + r1;
        r4_b <= (others => '0');
        r4_a <= "0000"&r1 + r4_a;

      when cycle_05 =>
        cycle <= cycle_06;

        if ra >= rd then 
          r0 <= ra;
          db <= "010"; --N
        else 
          r0 <= rd;
          db <= "110"; --NE
        end if;

        if r3 < r2 then 
          r3 <= r2;
          de <= dd;
        end if;

        r1 <= "0"&rb + rc;
        r2 <= "00"&r0 + r1;
        r4_a <= r4_a;
        r4_b <= "0000"&r1 + r4_b;
        
        r_out <= signed(unsigned(r4_a&"00")) - signed(unsigned(r4_a));

      when cycle_06 => 
        cycle <= cycle_07;

        if rc >= rf then 
          r0 <= rc;
          dc <= "000"; --E
        else 
          r0 <= rf;
          dc <= "101"; --SE
        end if;
        r3 <= r2;
        de <= da;

        r1 <= "0"&re + rd;
        r2 <= "00"&r0 + r1;
        r4_b <= "0000"&r1 + r4_b;

        -- output signal for "black" set
        o_valid <= '1';
        if (signed(unsigned("0"&r3&"000")) - r_out) > to_signed(383, 13) then
          o_edge <= '1';
          o_dir <= de;
        else 
          o_edge <= '0';
          o_dir <= "000";
        end if;

      when others => 
        if (i_valid or last_pixel) then
          cycle <= cycle_00;

        if re >= rh then 
          r0 <= re;
          dd <= "011"; --S
        else 
          r0 <= rh;
          dd <= "111"; --SW
        end if;

        if r3 < r2 then 
          r3 <= r2;
          de <= db;
        end if;
        
        r1 <= "0"&rf + rg;
        r2 <= "00"&r0 + r1;
        r4_a <= (others => '0');
        r4_b <= "0000"&r1 + r4_b;
        end if;
    end case;
  end if;
  end if;
end process;

end architecture main;
