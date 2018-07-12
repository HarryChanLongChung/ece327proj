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
end ostate_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;
use work.kirsch_synth_pkg.all;
use work.ostate_pkg.all;
use ieee.numeric_std_unsigned.all ;

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
  signal state : work.ostate_pkg.state_ty := resetState;

  signal col_index  : unsigned(7 downto 0) := "00000000";
  signal row_index  : unsigned(2 downto 0) := "000";

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

  signal ra : unsigned(7 downto 0);
  signal rb : unsigned(7 downto 0);
  signal rc : unsigned(7 downto 0);
  signal rd : unsigned(7 downto 0);
  signal re : unsigned(7 downto 0);
  signal rf : unsigned(7 downto 0);
  signal rg : unsigned(7 downto 0);
  signal rh : unsigned(7 downto 0);
  signal ri : unsigned(7 downto 0);

  signal row0_read : std_logic_vector(7 downto 0);
  signal row1_read : std_logic_vector(7 downto 0);
  signal row2_read : std_logic_vector(7 downto 0);

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
    

process
begin
wait until rising_edge(clk);
  if (reset = '1') then
    -- this is in reset for both the mode and our internal statemachine
    state   <= resetState;
    o_valid <= '0';
    o_mode  <= o_reset;

    col_index <= to_unsigned(0, 8);
    row_index <= to_unsigned(0, 3);

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
        if (i_valid) then
          -- at the end of any row
          if (col_index = to_unsigned(255, 8)) then
            row_wr_en <= row_wr_en rol 1;
            col_index <= to_unsigned(0,8);
            row_index <= row_index + 1;
          end if;

          -- finished filling up first 2 column on row 2
          if (col_index >= 2 and row_index >= 2) then
            rdy_calc <= '1';
          end if;
            --
          col_index <= col_index + 1;
          
            --update current input
          if (col_index = 0) then
            rg <= unsigned(i_pixel);
          elsif (col_index = 1) then
            rf <= unsigned(i_pixel);
          else 
            -- reassign intermediates
            ra <= rb;
            rb <= rc;
            rh <= ri;
            ri <= rd;
            rg <= rf;
            rf <= re;
            re <= unsigned(i_pixel);
          end if;

          case to_integer(unsigned(row_index)) is
            -- row0   a b c
            -- row1   h i d
            -- row2   g f e
            when 2 =>
            -- currently writing row 2
              if (col_index >= 2) then
                rc <= unsigned(row0_read);
                rd <= unsigned(row1_read);
              elsif (col_index = 1) then
                rh <= unsigned(row1_read);
                ra <= unsigned(row0_read);
              else 
                ri <= unsigned(row1_read);
                rb <= unsigned(row0_read);
              end if;
            when 1 =>
            -- currently writing row 1
              if (col_index >= 2) then
                rc <= unsigned(row2_read);
                rd <= unsigned(row0_read);
              elsif (col_index = 1) then
                rh <= unsigned(row0_read);
                ra <= unsigned(row2_read);
              else 
                ri <= unsigned(row0_read);
                rb <= unsigned(row2_read);
              end if;
            when 0 =>
            -- currently writing row 0
              if (col_index >= 2) then
                rc <= unsigned(row1_read);
                rd <= unsigned(row2_read);
              elsif (col_index = 1) then
                ra <= unsigned(row1_read);
                rh <= unsigned(row2_read);
              else 
                rb <= unsigned(row1_read);
                ri <= unsigned(row2_read);
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
  if (reset) then 
  end if;

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
      when others => 
    end case;
  end if;
end process;

end architecture main;
