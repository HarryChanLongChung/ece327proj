library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.numeric_std_unsigned.all ;

entity max is
  port (
    i_a, i_b	: in  std_logic_vector(9 downto 0);  -- input a and b
    o_val       : out std_logic_vector(9 downto 0);  -- output max(a,b)
    o_cmp       : out std_logic                      -- output 1 or 0
  );
end max;

architecture main of max is
begin 
    if (i_a > i_b) then
        o_val <= i_a;
        o_cmp <= '0';
    else if (i_a < i_b)
        o_val <= i_b;
        o_cmp <= '1';
    else 
        o_val <= i_b;
        o_cmp <= '0';
    end if;

end architecture main;