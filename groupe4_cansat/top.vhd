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
			--pwr_txd: OUT STD_LOGIC;
			rel_RX: IN STD_LOGIC;
			rel_TX: OUT STD_LOGIC;
			rel_MODE: OUT STD_LOGIC;
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
  
  -- main FSM
  type stateType is (
    idle, init, sendADCReset, wakeupADC, waitDataAvailable, readLow, readHigh, sendData, pause
  );
  signal state: stateType;
  signal nextState: stateType;
  signal ahbDone_s: std_Logic;
  signal enReadTransferAhb_s : std_ulogic;
  signal enWriteTransferAhb_s : std_ulogic;
  signal enTransferUART_s : std_ulogic;
  signal uart_done_s: std_logic;
  signal data_s: std_ulogic_vector (63 DOWNTO 0);
  signal reset_done_s: std_logic;
  
  
  -- AHB_read FSM
  type stateAHBType is (
    idle, readRequest, readTransfer, ending, writeRequest, writeTransfer
  );
  signal stateAHB: stateAHBType;
  signal nextStateAHB: stateAHBType;
  signal AHB_addr_read_s:  unsigned(ahbAddressBitNb-1 DOWNTO 0);
  --signal AHB_data_read_s: std_ulogic_vector (ahbDataBitNb-1 DOWNTO 0);
  signal AHB_addr_write_s:  unsigned(ahbAddressBitNb-1 DOWNTO 0);
  signal AHB_data_write_s: std_ulogic_vector (ahbDataBitNb-1 DOWNTO 0);
  
  -- Uart
  signal send_s : std_ulogic;
  signal rxData_s: std_ulogic_vector(uartDataBitNb-1 downto 0);
  signal txData_s: std_ulogic_vector(uartDataBitNb-1 downto 0);
  signal status_uart_s: std_ulogic_vector (uartStatusBitNb-1 DOWNTO 0);
  signal test_s:std_ulogic;
  type stateFSMuartType is (
    idle, send, sending, ending
  );
  signal stateUart: stateFSMuartType;
  signal nextStateUart: stateFSMuartType;
  signal shiftReg_s: std_ulogic_vector (63 DOWNTO 0);
  constant shiftRegisterSize: integer:=shiftReg_s'length/uartDataBitNb;
  signal shiftRegCounter_s: unsigned(4 DOWNTO 0);
                                                                         -- status
  constant statusReadyId: natural := 0;
  constant statusSendingId: natural := 1;
  constant statusReceivingId: natural := 2;
  
  -- Ads1282
  signal hAddr_s : unsigned(ahbAddressBitNb-1 DOWNTO 0);
  signal hSel_s : std_ulogic;
  signal hTrans_s : std_ulogic_vector(ahbTransBitNb-1 DOWNTO 0);
  signal hWData_s : std_ulogic_vector(ahbDataBitNb-1 DOWNTO 0);
  signal hWrite_s : std_ulogic;
  signal hRData_s : std_ulogic_vector(ahbDataBitNb-1 DOWNTO 0);
  signal hReady_s : std_ulogic;
  signal hResp_s : std_ulogic;
  signal ads_wakeUp_s: std_ulogic;
                                                           -- register definitions
  constant modulatorClockDividerRegisterId: natural := 0;
  constant spiClockDividerRegisterId: natural := 1;
  constant adcRegisterId: natural := 2;

  constant valueLowRegisterId: natural := 0;
  constant valueHighRegisterId: natural := 1;
  constant statusRegisterId: natural := 2;
  constant adcDataAvailableId: natural := 0;
  
  
  
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
            --pwr_leds(0)<='0';
				toogle_v:='0';
				counter_v := (others => '0');
        elsif rising_edge(clockIn) then
				if clk_1ms_s = '1' then
					if (counter_v < 500) then
						 --pwr_leds(0)<='0';
						 counter_v:=counter_v+1;
					elsif (counter_v < 1000) then
						 --pwr_leds(0)<='1';
						 counter_v:=counter_v+1;
					else
						counter_v := (others => '0');
					end if;
            end if;
        end if;
    end process;	 
	 
	--pwr_leds(1) <= status_uart_s(1);
	--pwr_leds(1)<='0';
	--pwr_leds(1) <=test_s;
	--pwr_leds(0)<=send_s;
	--pwr_txd<=test_s;
	--rel_TX<=test_s;
	---------------------------------------------------------------

	-- main FSM
  sequencer: process(state,clk_1ms_s,reset_done_s,ahbDone_s,hRData_s(adcDataAvailableId),uart_done_s,test_s)
  begin
      case state is
			when idle=>
				nextState <= init;
			when init =>
				if clk_1ms_s ='1' and reset_done_s ='0' then
					nextState <= sendADCReset;
				elsif clk_1ms_s ='1' and reset_done_s ='1' then
					nextState <= wakeupADC;
				else
					nextState <= init;
				end if;
			when sendADCReset =>
				if ahbDone_s = '1' then
					nextState <= init;
				else
					nextState <= sendADCReset;
				end if;
			when wakeupADC =>
					nextState <= waitDataAvailable;
					--nextState <= readLow;
			when waitDataAvailable =>
			 if ahbDone_s ='1' and hRData_s(adcDataAvailableId) ='1' then 
					nextState <= readLow;
			else
					nextState <= waitDataAvailable;
			end if;
			when readLow =>
				if ahbDone_s ='1' then 
					nextState <= readHigh;
				else
					nextState <= readLow;
        		end if;
			when readHigh =>
				if ahbDone_s ='1' then
					nextState <= sendData;
				else
					nextState <= readHigh;
				end if;
			when sendData => 
				if uart_done_s ='1' then
					--nextState <= wakeupADC; 
					nextState <= pause;
				else 
					nextState <= sendData;
				end if;
			when pause=>
				if test_s='1' then
				--if clk_1ms_s='1' then
					nextState<=wakeupADC;
				else
					nextState<=pause;
				end if;
      end case;
  end process sequencer;
  
  process(reset_n, clockIn)
  begin
    if reset_n = '0' then
      state<=idle;
    elsif rising_edge(clockIn) then
		state <= nextState;
	 end if;
  end process;
  
  -- FSM controls
  control: process(state)
  begin
	ads_wakeUp_s<='0';
	AHB_addr_read_s <=  (others =>'0');
	AHB_addr_write_s <= (others =>'0');
	AHB_data_write_s <= (others =>'0');
	enTransferUART_s<='0';
	enReadTransferAhb_s<='0';
	enWriteTransferAhb_s<='0';
    case state is
		when idle=>
			pwr_leds(0)<='0';
			pwr_leds(1) <='0';
		when init =>
			pwr_leds(0) <='1';
			pwr_leds(1) <='1';
		when sendADCReset =>
			enWriteTransferAhb_s<='1';
			AHB_addr_write_s <= to_unsigned(modulatorClockDividerRegisterId, AHB_addr_write_s'length);
			AHB_data_write_s <= std_ulogic_vector(to_unsigned(ADC_CLOCK_DIVIDER, AHB_data_write_s'length));
			pwr_leds(0) <='0';
			pwr_leds(1) <='1';
		when wakeupADC =>
			pwr_leds(0) <='0';
			pwr_leds(1) <='0';
			ads_wakeUp_s<='1';	
      when waitDataAvailable =>
			enReadTransferAhb_s<='1';
			AHB_addr_read_s <=  to_unsigned(statusRegisterId, AHB_addr_read_s'length);
			pwr_leds(0) <='1';
			pwr_leds(1) <='0';		
      when readLow =>
			enReadTransferAhb_s<='1';
			AHB_addr_read_s <=  to_unsigned(valueLowRegisterId, AHB_addr_read_s'length);
			pwr_leds(0) <='0';
			pwr_leds(1) <='1';
      when readHigh =>
			enReadTransferAhb_s<='1';
			AHB_addr_read_s <=  to_unsigned(valueHighRegisterId, AHB_addr_read_s'length);
			pwr_leds(0) <='1';
			pwr_leds(1) <='1';		
      when sendData => 
			enTransferUART_s<='1';
			pwr_leds(0) <='0';
			pwr_leds(1) <='0';	
		when pause =>
			pwr_leds(0) <='0';
			pwr_leds(1) <='0';
		end case;
  end process control;
  
  
  -- performe read data
  -- time from last sample, max = 2^32 * MCK_period =~ 40s
  process(reset_n, clockIn)
		variable counter_v : unsigned(31 downto 0);
		variable counter_updated: std_logic ;
  begin
    if reset_n = '0' then
      data_s <= (others=> '0');
		counter_v := (others => '0');
		counter_updated:='0';
    elsif rising_edge(clockIn) then
		if (state = readLow) then
			data_s(ahbDataBitNb-1 DOWNTO 0)<=hRData_s;
			if ( counter_updated ='0') then
				data_s(63 downto 32) <=std_ulogic_vector(counter_v); 
				counter_v := (others => '0');
				counter_updated:='1';
			end if;
		elsif (state = readHigh) then
			data_s(ahbDataBitNb*2-1 DOWNTO ahbDataBitNb)<=hRData_s;
			counter_v:=counter_v+1;
			counter_updated:='0';
		else
			data_s <= data_s;
			counter_v:=counter_v+1;
			counter_updated:='0';
		end if;
	end if;
	end process;
	
-- process enable ads
  process(reset_n, clockIn)
  begin
    if reset_n = '0' then
		reset_done_s <= '0';
    elsif rising_edge(clockIn) then
		if (state = sendADCReset) then
			reset_done_s <= '1';
		else 
			reset_done_s <= reset_done_s;
		end if;
	end if;
	end process;
---------------------------------------------------------------------------  
	-- only read Bus ahb FSM, no wait, 
  process(stateAHB,enReadTransferAhb_s,enWriteTransferAhb_s)
  begin
      case stateAHB is
        when idle =>
				 if  enReadTransferAhb_s ='1' then 
						nextStateAHB <= readRequest;
				elsif enWriteTransferAhb_s ='1' then
						nextStateAHB <= writeRequest;
				else
						nextStateAHB <= idle;
				end if;
			when readRequest =>
					nextStateAHB <= readTransfer;
			when readTransfer =>
					nextStateAHB <= ending;
			when writeRequest =>
					nextStateAHB <= writeTransfer;
			when writeTransfer =>
					nextStateAHB <= ending;
			when ending => 
					nextStateAHB <= idle;
      end case;
  end process;
  
  process(reset_n, clockIn)
  begin
    if reset_n = '0' then
      stateAHB<=idle;
    elsif rising_edge(clockIn) then
		stateAHB <= nextStateAHB;
	 end if;
  end process;
  
  process(stateAHB,AHB_addr_read_s,AHB_data_write_s,AHB_addr_write_s)
  begin
  hWData_s <=(others => '0');
    case stateAHB is
      when idle =>
			hSel_s<='0';
			ahbDone_s<='0';
			hwrite_s <='0';
			hAddr_s<=hAddr_s;
      when readRequest =>
			hSel_s<='1';
			ahbDone_s<='0';
			hwrite_s <='0';			
			hAddr_s<=AHB_addr_read_s;
      when readTransfer=>
			hSel_s<='0';
			ahbDone_s<='0';
			hwrite_s <='0';
			hAddr_s<=hAddr_s;
      when writeRequest =>
			hSel_s<='1';
			ahbDone_s<='0';
			hwrite_s <='1';			
			hAddr_s<=AHB_addr_write_s;
      when writeTransfer=>
			hSel_s<='0';
			ahbDone_s<='0';
			hwrite_s <='0';
			hAddr_s<=hAddr_s;
			hWData_s<=AHB_data_write_s;
      when ending => 
      	hSel_s<='0';
			ahbDone_s<='1';
			hwrite_s <='0';
			hAddr_s<=hAddr_s;
      when others => null;
    end case;
  end process control;
	
	
	
	------------------------------------------------------------------------
	-- transfer uart
	-- FSM uart
	
	process(reset_n, clockIn)
  begin
    if reset_n = '0' then
      stateUART<=idle;
    elsif rising_edge(clockIn) then
		stateUART <= nextStateUART;
	 end if;
  end process;
  
	sequencerUART: process(enTransferUART_s,stateUART,status_uart_s(statusSendingId))
  begin
      case stateUART is
        when idle =>
				if enTransferUART_s ='1' then 
					nextStateUART <= send;
				else
					nextStateUART <= idle;
				end if;
			when send =>
				nextStateUART<= sending;
			when sending =>
				if status_uart_s(statusSendingId) = '0' then
					if shiftRegCounter_s <8   then
						nextStateUART <= send;
					else 
						nextStateUART <= ending;
					end if;
				else
					nextStateUART <= sending;
				end if;
			when ending => 
					nextStateUART <= idle;
      end case;
  end process sequencerUART;
  
  -- FSM controls
  controlUART: process(stateUART,shiftReg_s)
  begin
    case stateUART is
      when idle =>
			send_s<='0';
			uart_done_s<='0';
			-- 
      when send =>
			send_s<='1';
			uart_done_s<='0';	
			txData_s <= shiftReg_s(uartDataBitNb-1 downto 0);	
      when sending =>
			send_s<='0';
			uart_done_s<='0';	
      when ending => 
      	uart_done_s<='1';
			send_s<='0';
      when others => null;
    end case;
  end process controlUART; 
  
  
  -- shift register counter
  process(reset_n, clockIn)
  begin
    if reset_n = '0' then
      shiftRegCounter_s<=(others => '0');
    elsif rising_edge(clockIn) then
		if(stateUart = send) then
			shiftReg_s(shiftReg_s'high-uartDataBitNb downto 0) <= shiftReg_s(shiftReg_s'high downto uartDataBitNb);
			shiftReg_s(shiftReg_s'high downto shiftReg_s'high-uartDataBitNb) <= (others =>'0');
			shiftRegCounter_s<=shiftRegCounter_s+1;
		elsif stateUart = sending then
			shiftRegCounter_s<=shiftRegCounter_s;
			shiftReg_s<=shiftReg_s;
		else
			shiftRegCounter_s<=(others=>'0');
			--shiftReg_s <= "00001000" & "00000111" & "00000110" & "00000101" & "00000100" & "00000011" & "00000010" & "00000001";
			shiftReg_s<=data_s;
		end if;
	 end if;
  end process;
	
	
	-------------------------------------------------------------
	-- components UART -------------s
	Inst_uartCore: uartCore PORT MAP(
		RxD    => rel_RX,
		--RxD => pwr_rxd,
		clock   => clockIn,
		read   => '0',
		reset  => NOT reset_n,
		scaler => to_unsigned(BAUDERATE_DIVIDER, ahbDataBitNb),
		send => send_s,
		txData => txData_s,
		--TxD    => pwr_txd,
		TxD => rel_TX,
		rxData => rxData_s,
		status => status_uart_s
  );
  
  --txData_s <= "01100001"; -- a 
  rel_MODE <='1'; 
  -----------------------------------------------------------
  --ads_wakeUp_s<='1';
  -- component ADC
   Inst_ads1282: ahbAds1282
  GENERIC MAP (
     dataBitNb => 8
  )  
  PORT MAP(
		DOUT     => sens_DOUT,
		DRDY_n   =>	sens_DRDY_n,
		enable   => ads_wakeUp_s,
		hAddr    =>  hAddr_s,
		hClk     =>	clockin,
		hReset_n => reset_n,
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
  
  hTrans_s<=transNonSeq;
  --hSel_s <='1';
  
  ------------------------------------------------------------
  
	process(clockIn, reset_n)
		variable counter_v : unsigned(31 downto 0);
	begin
	  if (reset_n = '0') then
			counter_v := (others => '0');
			test_s <='0';
	  elsif rising_edge(clockIn) then
			if clk_1ms_s = '1' then
				if (counter_v < 100) then
					 counter_v:=counter_v+1;
					 test_s <='0';
				else
					counter_v := (others => '0');
					test_s <='1';
				end if;
			else
				test_s <='0';
				counter_v := counter_v;
			end if;
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

