library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ostate_pkg is
  subtype mode_ty is std_logic_vector(1 downto 0);
  constant o_idle:  mode_ty := "10";
  constant o_busy:  mode_ty := "11";
  constant o_reset: mode_ty := "01";
end ostate_pkg;

package state_pkg is
  subtype state_ty is std_logic_vector(1 downto 0);
  constant resetState   : state_ty := "00";
  constant firstFill    : state_ty := "01";
  constant inputAndCount: state_ty := "10";
  constant result       : state_ty := "11";
end state_pkg;


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

  signal cycle : unsigned(2 downto 0) := "000";
  -- cycle 0: max(b,g)=r0,     max(a,d)=r1,     a+h=r2,        b+c=r3,      d+e=r4
  -- cycle 1: max(c,f)=r0,     max(e,h)=r1,     r0+r2=r5,    r1+r3=r6,  f+g=r7
  -- cycle 2: max(r5,r6)=r0,                   r2+r3=r2,    r0+r4=r3,  r1+r7=r5, 
  -- cycle 3: max(r1,r3)=r0, max(r2,r4)=r2
  -- cycle 4: max(r0,r4)=r0, max(r2,r7)=r2
  -- cycle 5: r0+"000"=r0, r2+"0"+r2=r2
  -- cycle 6: r0-r2=r2
  -- cycle 7: CMP(r2, 383)
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

  max0: entity WORK.max
    port map (
      o_val => max0_val,
      o_cmp => max0_cmp,
      i_a   => max0_a,
      i_b   => max0_b
    );

  max1: entity WORK.max
  port map (
    o_val => max1_val,
    o_cmp => max1_cmp,
    i_a   => max1_a,
    i_b   => max1_b
  );
    

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
  if (reset) then 
  end if

  if (rdy_calc) then 
    case cycle is 
      when "000" => 
      when "001" => 
      when "010" => 
      when "011" => 
      when "100" => 
      when "101" => 
      when "110" => 
      when "111" => 
    end case;
  end if;
end process;

end architecture main;
