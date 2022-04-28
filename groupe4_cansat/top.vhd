----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    18:33:36 04/28/2022 
-- Design Name: 
-- Module Name:    top - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity top is
	Port( pwr_leds: OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
			pwr_switches: IN STD_LOGIC_VECTOR (3 DOWNTO 0)
			);

end top;

architecture Behavioral of top is

begin

	pwr_leds(0) <= pwr_switches(0);
	pwr_leds(1) <= '1';

end Behavioral;

