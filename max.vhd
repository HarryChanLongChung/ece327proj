library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.numeric_std_unsigned.all ;

entity max is
  generic (
    data_width : natural := 8);
  port (
    i_a, i_b	: in  std_logic_vector(data_width - 1 downto 0);  -- input a and b
    i_clk       : in  std_logic;                     -- clk
    o_val       : out std_logic_vector(data_width - 1 downto 0);  -- output max(a,b)
    o_eqb       : out std_logic                      -- output 1 or 0
);
end max;

architecture main of max is
begin
    process
        begin 
        wait until rising_edge(i_clk);
            if (i_a > i_b) then
                o_val <= i_a;
                o_eqb <= '0';
            elsif (i_a < i_b) then
                o_val <= i_b;
                o_eqb <= '1';
            else 
                o_val <= i_a;
                o_eqb <= '0';
            end if;
        end process;

end architecture main;