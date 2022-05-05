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
use IEEE.NUMERIC_STD.ALL;
use work.constants.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity top is
	Port( pwr_leds: OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
			pwr_switches: IN STD_LOGIC_VECTOR (3 DOWNTO 0);
			clockIn: IN STD_LOGIC;				-- 106.25 MHz
			reset_n: IN STD_LOGIC			
			);

end top;

architecture Behavioral of top is

	signal clk_1ms_s: STD_LOGIC;
	signal elapseTime_s: unsigned(31 DOWNTO 0); -- overflow after 1193 hours
	--todo: add a counter clk cycle to add to ms to have it more accurate
begin

	-- process counter ms
		counter_ms : process(clockIn, reset_n)
		variable counter_v : unsigned(31 downto 0);
      begin
        if (reset_n = '0') then
            counter_v := (others => '0');
            clk_1ms_s <= '0';
				elapseTime_s <= (others => '0');
        elsif rising_edge(clockIn) then
            if (counter_v < CLOCK_DIVIDER_1MS) then
                clk_1ms_s <= '0';
					 counter_v:= counter_v+1;
            else
					 elapseTime_s<=elapseTime_s+1;
					 clk_1ms_s <= '1';
					 counter_v:= (others => '0');
				end if;
        end if;
    end process;
	 
	 -- process blinking led
		blinking_led : process(clockIn, reset_n)
		variable counter_v : unsigned(31 downto 0);
		variable toogle_v: STD_LOGIC;
      begin
        if (reset_n = '0') then
            pwr_leds(0)<='0';
				toogle_v:='0';
				counter_v := (others => '0');
        elsif rising_edge(clockIn) then
				if clk_1ms_s = '1' then
					if (counter_v < 500) then
						 pwr_leds(0)<='0';
						 counter_v:=counter_v+1;
					elsif (counter_v < 1000) then
						 pwr_leds(0)<='1';
						 counter_v:=counter_v+1;
					else
						counter_v := (others => '0');
					end if;
            end if;
        end if;
    end process;	 
	 

	 
	pwr_leds(1) <= pwr_switches(0);
	
	

end Behavioral;

