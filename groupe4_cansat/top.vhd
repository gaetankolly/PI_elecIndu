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
			reset_n: IN STD_LOGIC;
			--pwr_rxd: IN STD_LOGIC;
			--pwr_txd: OUT STD_LOGIC
			rel_RX: IN STD_LOGIC;
			rel_TX: OUT STD_LOGIC;
			rel_MODE: OUT STD_LOGIC
			-- Ads128
			sens_SCLK: OUT STD_LOGIC;
			sens_DOUT: IN STD_LOGIC;
			sens_DIN: OUT STD_LOGIC;
			sens_DRDY_n: IN STD_LOGIC;
			sens_SYNC: OUT STD_LOGIC;
			sens_PWDN_n: OUT STD_LOGIC;
			sens_CLK: OUT STD_LOGIC;
			sens_RESET_n: OUT STD_LOGIC
			);
end top;

architecture Behavioral of top is

	signal clk_1ms_s: STD_LOGIC;
	signal elapseTime_s: unsigned(31 DOWNTO 0); -- overflow after 1193 hours
	--todo: add a counter clk cycle to add to ms to have it more accurate

	-- components
	COMPONENT uartCore 
	PORT( 
		RxD    : IN     std_ulogic;
		clock  : IN     std_uLogic;
		read   : IN     std_uLogic;
		reset  : IN     std_uLogic;
		scaler : IN     unsigned ( ahbDataBitNb-1 DOWNTO 0 );
		send   : IN     std_uLogic;
		txData : IN     std_ulogic_vector (uartDataBitNb-1 DOWNTO 0);
		TxD    : OUT    std_ulogic;
		rxData : OUT    std_ulogic_vector (uartDataBitNb-1 DOWNTO 0);
		status : OUT    std_ulogic_vector (uartStatusBitNb-1 DOWNTO 0)
  );
  END COMPONENT;
  
  COMPONENT ahbAds1282
  GENERIC (
      dataBitNb : positive := 8
   );
  PORT(
      DOUT     : IN     std_uLogic;
		DRDY_n   : IN     std_ulogic;
		enable   : IN     std_uLogic;
		hAddr    : IN     unsigned ( ahbAddressBitNb-1 DOWNTO 0 );
		hClk     : IN     std_uLogic;
		hReset_n : IN     std_uLogic;
		hSel     : IN     std_uLogic;
		hTrans   : IN     std_ulogic_vector (ahbTransBitNb-1 DOWNTO 0);
		hWData   : IN     std_ulogic_vector (ahbDataBitNb-1 DOWNTO 0);
		hWrite   : IN     std_uLogic;
		CLK      : OUT    std_uLogic;
		DIN      : OUT    std_uLogic;
		PWDN_n   : OUT    std_uLogic;
		RESET_n  : OUT    std_uLogic;
		SCLK     : OUT    std_uLogic;
		SYNC     : OUT    std_uLogic;
		hRData   : OUT    std_ulogic_vector (ahbDataBitNb-1 DOWNTO 0);
		hReady   : OUT    std_uLogic;
		hResp    : OUT    std_uLogic
  );
  END COMPONENT;
  
  -- Uart
  signal send_s : std_ulogic;
  signal rxData_s: std_ulogic_vector(uartDataBitNb-1 downto 0);
  signal txData_s: std_ulogic_vector(uartDataBitNb-1 downto 0);
  signal status_uart_s: std_ulogic_vector (uartStatusBitNb-1 DOWNTO 0);
  signal test_s:std_ulogic;
  
  -- Ads1282
  signal hAddr_s : unsigned(ahbAddressBitNb-1 DOWNTO 0);
  signal hSel_s : std_ulogic;
  signal hTrans_s : std_ulogic_vector(ahbTransBitNb-1 DOWNTO 0);
  signal hWData_s : std_ulogic_vector(ahbDataBitNb-1 DOWNTO 0);
  signal hWrite_s : std_ulogic;
  signal hRData_s : std_ulogic_vector(ahbDataBitNb-1 DOWNTO 0);
  signal hReady_s : std_ulogic;
  signal hResp_s : std_ulogic;
  
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
	 
	--pwr_leds(1) <= status_uart_s(1);
	pwr_leds(1)<='0';
	--pwr_leds(1) <=test_s;
	--pwr_leds(0)<=send_s;
	--pwr_txd<=test_s;
	--rel_TX<=test_s;
	
	-- components UART -------------s
	Inst_uartCore: uartCore PORT MAP(
		RxD    => pwr_rxd,
		clock   => clockIn,
		read   => '0',
		reset  => NOT reset_n,
		scaler => to_unsigned(BAUDERATE_DIVIDER, ahbDataBitNb),
		send => rel_TX,
		txData => txData_s,
		--TxD    => pwr_txd,
		TxD => test_s,
		rxData => rxData_s,
		status => status_uart_s
  );
  
  txData_s <= "01100001"; -- a 
  rel_MODE <='1'; 
  
  -- component ADC
   Inst_ads1282: ahbAds1282
  GENERIC MAP (
     dataBitNb => 8
  )  
  PORT MAP(
		DOUT     => sens_DOUT,
		DRDY_n   =>	sens_DRDY_n,
		enable   =>	'1',
		hAddr    =>  hAddr_s,
		hClk     =>	clockin,
		hReset_n => NOT reset_n,
		hSel     => hSel_s,
		hTrans   => hTrans_s,
		hWData   => hWData_s,
		hWrite   => hWrite_s,
		CLK      =>	sens_CLK,
		DIN      => sens_DIN,
		PWDN_n   => sens_PWDN_n,
		RESET_n  => sens_RESET_n,
		SCLK     => sens_SCLK,
		SYNC     => sens_SYNC,
		hRData   => hRData_s,
		hReady   => hReady_s,
		hResp    => hResp_s
  );
  
  
  
	process(clockIn, reset_n)
		variable counter_v : unsigned(31 downto 0);
	begin
	  if (reset_n = '0') then
			send_s<='0';
			counter_v := (others => '0');
	  elsif rising_edge(clockIn) then
			if clk_1ms_s = '1' then
				if (counter_v < 1000) then
					 counter_v:=counter_v+1;
				else
					send_s<='1';
					counter_v := (others => '0');
				end if;
			else
				send_s<='0';
			end if;
	  end if;
	end process;


	process(clockIn, reset_n)
	begin
		if (reset_n = '0') then
		
		elsif rising_edge(clockIn) then
			
		end if;
	end process;

end Behavioral;

PACKAGE BODY constants IS

	  function addressBitNb (addressNb : natural) return natural is
		 variable powerOfTwo, bitNb : natural;
	  begin
		 powerOfTwo := 1;
		 bitNb := 0;
		 while powerOfTwo <= addressNb loop
			powerOfTwo := 2 * powerOfTwo;
			bitNb := bitNb + 1;
		 end loop;
		 return bitNb;
	  end addressBitNb;

END constants;

