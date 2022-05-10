library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.ALL;


package constants is

constant ahbDataBitNb : integer:= 32;
constant uartDataBitNb: integer:= 8;
constant uartStatusBitNb: integer:= 4;

constant CLOCK_DIVIDER_1MS: integer:= 106250;
constant BAUDERATE_DIVIDER : integer:= 922; -- clk/bauderate, 922=115200
--constant BAUDERATE_DIVIDER : integer:= 10625000;


end constants;
