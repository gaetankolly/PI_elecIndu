-- VHDL Entity AhbLiteComponents.ahbAds1282.symbol
--
-- Created:
--          by - francois.francois (Aphelia)
--          at - 14:40:14 02/19/19
--
-- Generated by Mentor Graphics' HDL Designer(TM) 2018.1 (Build 12)
--
LIBRARY ieee;
  USE ieee.std_logic_1164.all;
  USE ieee.numeric_std.all;

use work.constants.all;

ENTITY ahbAds1282 IS
  GENERIC( 
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
    CLK      : OUT    std_uLogic;			-- modulator clock
    DIN      : OUT    std_uLogic;
    PWDN_n   : OUT    std_uLogic;
    RESET_n  : OUT    std_uLogic;
    SCLK     : OUT    std_uLogic;
    SYNC     : OUT    std_uLogic;
    hRData   : OUT    std_ulogic_vector (ahbDataBitNb-1 DOWNTO 0);
    hReady   : OUT    std_uLogic;
    hResp    : OUT    std_uLogic
  );

-- Declarations

END ahbAds1282 ;

ARCHITECTURE RTL OF ahbAds1282 IS

  signal reset, clock: std_ulogic;
                                                         -- register definitions
  constant modulatorClockDividerRegisterId: natural := 0;
  constant spiClockDividerRegisterId: natural := 1;
  constant adcRegisterId: natural := 2;

  constant valueLowRegisterId: natural := 0;
  constant valueHighRegisterId: natural := 1;
  constant statusRegisterId: natural := 2;
  constant adcDataAvailableId: natural := 0;

  constant registerNb: positive := adcRegisterId+1;
  constant registerAddresssBitNb: positive := addressBitNb(registerNb);
  signal addressReg: unsigned(registerAddresssBitNb-1 downto 0);
  signal writeReg: std_ulogic;
                                                            -- control registers
  subtype registerType is unsigned(hWdata'range);
  type registerArrayType is array (registerNb-1 downto 0) of registerType;
  signal writeRegisterArray: registerArrayType;
                                                      -- modulator clock divider
  signal modulatorDividerCounter: unsigned(registerType'range);
  constant modulatorRestartCounterBitNb: positive := 3;
  signal modulatorRestartCounter: unsigned(
    modulatorRestartCounterBitNb downto 0
  );
  signal modulatorClockEn, modulatorClock: std_ulogic;
                                                            -- SPI clock divider
  signal spiDividerCounter: unsigned(registerType'range);
  signal spiClockEn, spiClock: std_ulogic;
                                                           -- ADC register write
  constant adcRegisterAddressBitNb: positive := 4;
  constant adcRegisterDataBitNb: positive := 8;
  signal adcRegisterWriteAddress: unsigned(adcRegisterAddressBitNb-1 downto 0);
  signal adcRegisterWriteValue  : unsigned(adcRegisterDataBitNb-1 downto 0);
                                                               -- ADC SPI access
  constant adcSpiCommandBitNb: positive := 8;
    constant cmdWakeup  : unsigned(adcSpiCommandBitNb-1 downto 0) := x"00";
    constant cmdStandby : unsigned(adcSpiCommandBitNb-1 downto 0) := x"02";
    constant cmdWriteReg: unsigned(adcSpiCommandBitNb-1 downto 0) := x"40";
    constant regConfig0 : unsigned(adcSpiCommandBitNb-1 downto 0) := x"01";
    constant regConfig1 : unsigned(adcSpiCommandBitNb-1 downto 0) := x"02";
  constant adcSpiDataBitNb: positive := 32;
  signal adcCommand: unsigned(adcSpiCommandBitNb-1 downto 0);
  signal adcSendCommand, adcSendRead, adcSending: std_ulogic;
  signal adcSpiDataOut: unsigned(adcCommand'high+1 downto 0);
  signal adcSpiDataIn, adcSample: unsigned(adcSpiDataBitNb-1 downto 0);
  signal adcSpiCounter: unsigned(addressBitNb(adcSpiDataBitNb)-1 downto 0);

                                                                          -- FSM
  type adcStateType is (
    waitSample,
    sendWakeup, waitDataReady, startRead, waitRead, reading, sendStandby
  );
  signal adcState: adcStateType;
                                                              -- status register
  signal isReadAccess: std_ulogic;
  signal adcSetDataAvailable, adcClrDataAvailable, adcDataAvailable: std_ulogic;
  signal adcStatusRegisterSet, adcStatusRegisterClear: registerType;
  signal adcStatusRegister: registerType;

BEGIN

  ------------------------------------------------------------------------------
                                                              -- reset and clock
  reset <= not hReset_n;
  clock <= hClk;

  --============================================================================
                                                         -- address and controls
  storeControls: process(reset, clock)
  begin
    if reset = '1' then
      addressReg <= (others => '0');
      writeReg <= '0';
      isReadAccess <= '0';
    elsif rising_edge(clock) then
      writeReg <= '0';
      isReadAccess <= '0';
      if (hSel = '1') and (hTrans = transNonSeq) then
        addressReg <= hAddr(addressReg'range);
        writeReg <= hWrite;
        isReadAccess <= not hWrite;
      end if;
    end if;
  end process storeControls;

  ------------------------------------------------------------------------------
                                                                    -- registers
  storeRegisters: process(reset, clock)
  begin
    if reset = '1' then
      writeRegisterArray <= (others => (others => '0'));
      writeRegisterArray(modulatorClockDividerRegisterId) <= to_unsigned(
        2,
        writeRegisterArray(modulatorClockDividerRegisterId)'length
      );
      writeRegisterArray(spiClockDividerRegisterId) <= to_unsigned(
        2,
        writeRegisterArray(spiClockDividerRegisterId)'length
      );
    elsif rising_edge(clock) then
      if writeReg = '1' AND addressReg = adcRegisterId then
        writeRegisterArray(to_integer(addressReg)) <= unsigned(hWData);
      end if;
		writeRegisterArray(modulatorClockDividerRegisterId) <= to_unsigned(
			ADC_CLOCK_DIVIDER, writeRegisterArray(modulatorClockDividerRegisterId)'length);
		writeRegisterArray(spiClockDividerRegisterId)  <= to_unsigned(
			SPI_CLOCK_DIVIDER, writeRegisterArray(spiClockDividerRegisterId)'length);
    end if;
  end process storeRegisters;

  --============================================================================
                                                      -- modulator clock divider
  countHalfModulatorPeriod: process(reset, clock)
    variable maxCounterValue: natural;
  begin
    if reset = '1' then
      modulatorDividerCounter <= (others => '0');
      modulatorClockEn <= '0';
    elsif rising_edge(clock) then
      modulatorClockEn <= '0';
                                               -- set count value to half period
      maxCounterValue := to_integer(
        writeRegisterArray(modulatorClockDividerRegisterId)/2 - 1
      );
                                       -- add 1 clock period fo odd count values
      if (writeRegisterArray(modulatorClockDividerRegisterId)(0) = '1') and
          (modulatorClock = '1') then
        maxCounterValue := maxCounterValue + 1;
      end if;
                                                               -- period counter
      if modulatorDividerCounter < maxCounterValue then
        modulatorDividerCounter <= modulatorDividerCounter + 1;
      else
        modulatorDividerCounter <= (others => '0');
        modulatorClockEn <= '1';
      end if;
    end if;
  end process countHalfModulatorPeriod;

  divideModulatorClock: process(reset, clock)
  begin
    if reset = '1' then
      modulatorClock <= '0';
    elsif rising_edge(clock) then
      if modulatorClockEn = '1' then
        modulatorClock <= not modulatorClock;
      end if;
    end if;
  end process divideModulatorClock;

  CLK <= modulatorClock;

  ------------------------------------------------------------------------------
                                                        -- restart pulse for ADC
  delayRestart: process(reset, clock)
  begin
    if reset = '1' then
      modulatorRestartCounter <= (others => '0');
    elsif rising_edge(clock) then
      if (writeReg = '1') and (addressReg = modulatorClockDividerRegisterId) then
        modulatorRestartCounter <= (others => '1');
      elsif (modulatorClockEn = '1') and (modulatorRestartCounter > 0) then
        modulatorRestartCounter <= modulatorRestartCounter - 1;
      end if;
    end if;
  end process delayRestart;

  RESET_n <= not '0' when modulatorRestartCounter = 0
    else not '1';

  ------------------------------------------------------------------------------
                                                            -- SPI clock divider
  countHalfSpiPeriod: process(reset, clock)
    variable maxCounterValue: natural;
  begin
    if reset = '1' then
      spiDividerCounter <= (others => '0');
      spiClockEn <= '0';
    elsif rising_edge(clock) then
      spiClockEn <= '0';
                                               -- set count value to half period
      maxCounterValue := to_integer(
        writeRegisterArray(spiClockDividerRegisterId)/2 - 1
      );
                                       -- add 1 clock period fo odd count values
      if (writeRegisterArray(spiClockDividerRegisterId)(0) = '1') and
          (spiClock = '0') then
        maxCounterValue := maxCounterValue + 1;
      end if;
                                                               -- period counter
      if (modulatorClockEn = '1') and (modulatorClock = '0') then
        if spiDividerCounter < maxCounterValue then
          spiDividerCounter <= spiDividerCounter + 1;
        else
          spiDividerCounter <= (others => '0');
          spiClockEn <= '1';
        end if;
      end if;
    end if;
  end process countHalfSpiPeriod;

  divideSpiClock: process(reset, clock)
  begin
    if reset = '1' then
      spiClock <= '0';
    elsif rising_edge(clock) then
      if spiClockEn = '1' then
        spiClock <= not spiClock;
      end if;
    end if;
  end process divideSpiClock;

  --============================================================================
                                                           -- ADC register write
  adcRegisterWriteValue <= writeRegisterArray(adcRegisterId)(adcRegisterWriteValue'range);
  adcRegisterWriteAddress <= writeRegisterArray(adcRegisterId)(
    adcRegisterWriteValue'length+adcRegisterWriteAddress'length-1 downto adcRegisterWriteValue'length
  );

--  signalRegisterAccess: process(reset, clock)
--  begin
--    if reset = '1' then
--      adcSendCommand <= '0';
--      adcCommand <= (others => '0');
--    elsif rising_edge(clock) then
--      adcSendCommand <= '0';
--      if (writeReg = '1') and (addressReg = adcRegisterId) then
--        adcSendCommand <= '1';
--        adcCommand <= unsigned(hwdata(adcCommand'range));
--      end if;
--    end if;
--  end process signalRegisterAccess;

  --============================================================================
                                                                      -- ADC FSM
  adcSequencer: process(reset, clock)
  begin
    if reset = '1' then
      adcState <= waitSample;
    elsif rising_edge(clock) then
      case adcState is
        when waitSample =>
          if enable = '1' then
            if DRDY_n = not '0' then
              adcState <= sendWakeup;
--              adcState <= waitDataReady;
            else
              adcState <= startRead;
            end if;
          elsif modulatorRestartCounter = 1 then
            adcState <= sendStandby;
          end if;
        when sendWakeup =>
          adcState <= waitDataReady;
        when waitDataReady =>
          if DRDY_n = not '1' then
            adcState <= startRead;
          elsif enable = '1' then
            adcState <= waitSample;
          end if;
        when startRead =>
          adcState <= waitRead;
        when waitRead =>
          if adcSending = '1' then
            adcState <= reading;
          end if;
        when reading =>
          if adcSending = '0' then
            adcState <= sendStandby;
          end if;
        when sendStandby =>
          adcState <= waitSample;
        -- when others => null;
      end case;
      if (writeReg = '1') and (addressReg = spiClockDividerRegisterId) then
        adcState <= sendWakeup;
      end if;
    end if;
  end process adcSequencer;
                                                                 -- ADC controls
  adcControls: process(adcState)
  begin
    adcSendCommand <= '0';
    adcCommand <= (others => '0');
    adcSendRead <= '0';
    adcSetDataAvailable <= '0';
    case adcState is
      when sendWakeup =>
        adcSendCommand <= '1';
        adcCommand <= cmdWakeup;
      when startRead =>
        adcSendRead <= '1';
      when sendStandby =>
        adcSendCommand <= '1';
        adcCommand <= cmdStandby;
        adcSetDataAvailable <= '1';
      when others => null;
    end case;
  end process adcControls;

  SYNC <= '0';
  PWDN_n <= not '0';

  --============================================================================
                                                               -- ADC SPI access
  spiExchangeData: process(reset, clock)
  begin
    if reset = '1' then
      adcSpiDataOut <= (others => '0');
      adcSpiDataIn <= (others => '0');
      adcSample <= (others => '0');
      adcSpiCounter <= (others => '0');
      adcSending <= '0';
    elsif rising_edge(clock) then
      if adcSpiCounter = 0 then
        adcSending <= '0';
        if adcSendCommand = '1' then
          adcSpiCounter <= to_unsigned(adcSpiCommandBitNb+1, adcSpiCounter'length);
          adcSpiDataOut <= shift_left(
            resize(adcCommand, adcSpiDataOut'length),
            adcSpiDataOut'length - adcCommand'length - 1
          );
        elsif adcSendRead = '1' then
          adcSpiCounter <= to_unsigned(adcSpiDataBitNb+1, adcSpiCounter'length);
        end if;
        if adcState = reading then
          adcSample <= adcSpiDataIn;
        end if;
      elsif (spiClockEn = '1') and (spiClock = '1') then
        adcSpiCounter <= adcSpiCounter - 1;
        adcSpiDataOut <= shift_left(adcSpiDataOut, 1);
        adcSpiDataIn  <= shift_left(adcSpiDataIn, 1);
        adcSpiDataIn(0) <= DOUT;
        if (adcSpiCounter = adcSpiCommandBitNb+1) or
            (adcSpiCounter = adcSpiDataBitNb+1) then
          adcSending <= '1';
        end if;
      end if;
    end if;
  end process spiExchangeData;

  SCLK <= adcSending and spiClock;
  DIN <= adcSpiDataOut(adcSpiDataOut'high);

  --============================================================================
                                                                -- data readback
  selectData: process(addressReg, adcSample, adcStatusRegister)
  begin
    case to_integer(addressReg) is
      when valueLowRegisterId =>
        --hRData <= std_ulogic_vector(adcSample(hRData'range));
		  hRdata <=   "00000100" & "00000011" & "00000010" & "00001001";
      when valueHighRegisterId =>
        --hRData <= std_ulogic_vector( shift_right(adcSample, hRData'length)(hRData'range));
		  hRdata <= "00001000" & "00000111" & "00000110" & "00000101";
      when statusRegisterId =>
        hRData <= std_ulogic_vector(adcStatusRegister);
		  --hRdata <= "00001000" & "00000111" & "00000110" & "00000101";
      when others => hRData <= (others => '-');
    end case;
  end process selectData;
                                                              -- status register
  adcStatusRegisterClear(adcDataAvailableId) <= '1' when
      (isReadAccess = '1') and (to_integer(addressReg) = valueHighRegisterId)
    else '0';

    adcClrDataAvailable <= '1' when
      (isReadAccess = '1') and (to_integer(addressReg) = valueHighRegisterId)
    else '0';

  updateDataAvailable: process(reset, clock)
  begin
    if reset = '1' then
      adcDataAvailable <= '0';
    elsif rising_edge(clock) then
      if adcSetDataAvailable = '1' then
        adcDataAvailable <= '1';
      elsif adcClrDataAvailable = '1' then
        adcDataAvailable <= '0';
      end if;
    end if;
  end process updateDataAvailable;

  buildStatusRegister: process (adcDataAvailable)
  begin
    adcStatusRegister <= (others => '-');
    adcStatusRegister(adcDataAvailableId) <= adcDataAvailable;
  end process buildStatusRegister;

  hReady <= '1';  -- no wait state
  hResp  <= '0';  -- data OK

END ARCHITECTURE RTL;
