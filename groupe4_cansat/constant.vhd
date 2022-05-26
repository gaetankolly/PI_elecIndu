library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.ALL;


package constants is

constant ahbDataBitNb : integer:= 16;
--constant uartDataBitNb: integer:= 8;
constant uartDataBitNb: integer:= 8;
constant uartStatusBitNb: integer:= 4;

constant MASTER_CLOCK : integer:= 106250000; --Hz
constant CLOCK_DIVIDER_1MS: integer:= 106250;
constant BAUDERATE_DIVIDER : integer:= 922; -- clk/bauderate, 922=115200
--constant BAUDERATE_DIVIDER : integer:= 10625000;

--constant ADC_CLOCK_FREQ: integer:= 2000000;
constant ADC_CLOCK_FREQ: integer:= 4096000;
constant ADC_CLOCK_DIVIDER: integer:= MASTER_CLOCK/ADC_CLOCK_FREQ;
--constant SPI_CLOCK_DIVIDER: integer:= MASTER_CLOCK/(ADC_CLOCK_FREQ/4);
constant SPI_CLOCK_DIVIDER: integer:= 2; --modulator/4

constant ahbAddressBitNb  : positive := 16;
--constant ahbDataBitNb     : positive := 16;
constant ahbSlaveNb       : positive := 16;

constant ahbTransBitNb    : positive := 2;
constant ahbSizeBitNb     : positive := 1;
constant ahbBurstBitNb    : positive := 3;
constant ahbProtBitNb     : positive := 4;

------------------------------------------------------------------------------
																		-- bus data vector type
subtype ahbDataType is std_logic_vector(ahbDataBitNb-1 downto 0);
type ahbDataVector is array(1 to ahbSlaveNb) of ahbDataType;

------------------------------------------------------------------------------
																			  -- address decoder
type ahbMemoryLocationType is
 record
	baseAddress: natural;
	addressMask: natural;
 end record;
type ahbMemoryLocationVector is array(1 to ahbSlaveNb) of ahbMemoryLocationType;

------------------------------------------------------------------------------
																					-- bus signals
subtype transferType is std_ulogic_vector(ahbTransBitNb-1 downto 0);
constant transIdle  : transferType := "00";
constant transBusy  : transferType := "01";
constant transNonSeq: transferType := "10";
constant transSeq   : transferType := "11";

subtype transferSizeType is std_ulogic_vector(ahbSizeBitNb-1 downto 0);
constant size8   : transferSizeType := "0";
constant size16  : transferSizeType := "1";

subtype burstType is std_ulogic_vector(ahbBurstBitNb-1 downto 0);
constant burstSingle : burstType := "000";
constant burstIncr   : burstType := "001";
constant burstWrap4  : burstType := "010";
constant burstIncr4  : burstType := "011";
constant burstWrap8  : burstType := "100";
constant burstIncr8  : burstType := "101";
constant burstWrap16 : burstType := "110";
constant burstIncr16 : burstType := "111";

subtype protectionType is std_ulogic_vector(ahbProtBitNb-1 downto 0);
constant protDefault : protectionType := "0011";

------------------------------------------------------------------------------
																							 -- log2
function addressBitNb (addressNb : natural) return natural;

--- Our Constants ---


end constants;
