library ieee;
use ieee.std_logic_1164.all;

entity mult_3 is
  port ( i: in  std_logic_vector(11 downto 0);
         o: out std_logic_vector(13 downto 0)
  );
end mult_3;

architecture main of mult_3  is
begin
  i <= i & '0';
  o <= i + i;

end architecture;
