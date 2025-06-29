library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity AmplitudeEqualizer is
    Port (
        clk                 : in  STD_LOGIC;                      -- System clock
        rst                 : in  STD_LOGIC;                      -- Active high reset
        x_in                : in  STD_LOGIC_VECTOR(7 downto 0);   -- 8-bit signed input sample
        ref                 : in  STD_LOGIC_VECTOR(15 downto 0);  -- Reference amplitude (1023)
        y_out               : out STD_LOGIC_VECTOR(7 downto 0);   -- 8-bit signed output

        -- Debug ports for verification
        dbg_peak_detected   : out STD_LOGIC;
        dbg_peak_value      : out STD_LOGIC_VECTOR(7 downto 0);
        dbg_rho             : out STD_LOGIC_VECTOR(7 downto 0);
        dbg_valid           : out STD_LOGIC;                      -- Pipeline valid signal
        dbg_overflow        : out STD_LOGIC                       -- Rho calculation overflow flag
    );
end AmplitudeEqualizer;

architecture Behavioral of AmplitudeEqualizer is
    -- Constants
    constant RHO_INIT       : signed(7 downto 0) := to_signed(1, 8);  -- Initial rho = 1 / can be set any
    constant REF_VALUE      : signed(15 downto 0) := to_signed(1023, 16); -- Fixed reference (2^10-1)

    -- Sample pipeline for peak detection (current + adjacent samples)
    signal x_prev, x_now, x_next : signed(7 downto 0) := (others => '0');

    -- Input pipeline for synchronization with peak detection
    signal x_in_current     : signed(7 downto 0) := (others => '0');

    -- Pipeline valid signal (high after pipeline is filled)
    signal pipeline_valid   : std_logic := '0';
    signal sample_count     : unsigned(2 downto 0) := (others => '0'); 

    -- Peak detection signals
    signal peak_detected    : std_logic := '0';
    signal peak_value       : signed(7 downto 0) := (others => '0');

    -- Correction factor signals
    signal rho              : signed(7 downto 0) := RHO_INIT;
    signal rho_overflow     : std_logic := '0';

    -- Multiplication signals
    signal input_times_rho  : signed(15 downto 0) := (others => '0');
    signal acc_output       : signed(19 downto 0) := (others => '0'); -- 20-bit accumulator

begin

    ---------------------------------------------------------------------------
    -- Sample Pipeline Process
    -- Maintains a 3-sample window for peak detection
    ---------------------------------------------------------------------------
    sample_pipeline: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                x_prev <= (others => '0');
                x_now <= (others => '0');
                x_next <= (others => '0');
                x_in_current <= (others => '0');
                sample_count <= (others => '0');
                pipeline_valid <= '0';
            else
                -- Input synchronization pipeline
                x_in_current <= signed(x_in);

                -- Shift new sample through peak detection pipeline
                x_prev <= x_now;
                x_now <= x_next;
                x_next <= signed(x_in);

                -- Count samples until pipeline is filled
                if pipeline_valid = '0' then
                    if sample_count = 3 then  -- Need 4 samples for valid synchronized detection
                        pipeline_valid <= '1';
                    else
                        sample_count <= sample_count + 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

    ---------------------------------------------------------------------------
    -- Peak Detection Process
    -- Detects both positive and negative peaks in the triangle wave
    -- Peaks occur when the middle sample is greater/less than both neighbors
    ---------------------------------------------------------------------------
    peak_detection: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                peak_detected <= '0';
                peak_value <= (others => '0');
            else
                -- Default no peak detected
                peak_detected <= '0';

                -- Only detect peaks when pipeline is valid
                if pipeline_valid = '1' then
                    -- Positive peak: x_prev < x_now > x_next
                    if (x_prev < x_now) and (x_now > x_next) then
                        peak_detected <= '1';
                        peak_value <= x_now;

                    -- Negative peak: x_prev > x_now < x_next
                    elsif (x_prev > x_now) and (x_now < x_next) then
                        peak_detected <= '1';
                        peak_value <= x_now;
                    end if;
                end if;
            end if;
        end if;
    end process;

    ---------------------------------------------------------------------------
    -- Rho Update Process (Correction Factor)
    -- Updates rho only at peaks using: rho_k+1 = rho_k + (R - |Pk*rho_k|)
    -- Includes saturation logic to keep rho within 8-bit signed range
    ---------------------------------------------------------------------------
    rho_update: process(clk)
        variable peak_rho_product : signed(15 downto 0);
        variable abs_product      : signed(15 downto 0);
        variable correction       : signed(15 downto 0);
        variable next_rho         : signed(15 downto 0);
        variable overflow_detected : std_logic;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                rho <= RHO_INIT;
                rho_overflow <= '0';
            else
                -- Default: no overflow
                overflow_detected := '0';

                -- Only update at peaks and when pipeline is valid
                if (peak_detected = '1') and (pipeline_valid = '1') then
                    -- Calculate Pk* rho_k (16-bit result)
                    peak_rho_product := peak_value * rho;

                    -- Get absolute value |Pk* rho_k|
                    if peak_rho_product < 0 then
                        abs_product := -peak_rho_product;
                    else
                        abs_product := peak_rho_product;
                    end if;

                    -- Calculate (R - |Pk*rho_k|)
                    correction := REF_VALUE - abs_product;

                    -- Apply full formula: rho_k+1 = rho_k + (R - |Pk*rho_k|)
                    next_rho := resize(rho, 16) + correction;

                    -- Check for overflow
                    if next_rho > 127 then
                        rho <= to_signed(127, 8); -- Max positive
                        overflow_detected := '1';
                    elsif next_rho < -128 then
                        rho <= to_signed(-128, 8); -- Min negative
                        overflow_detected := '1';
                    else
                        rho <= next_rho(7 downto 0);
                    end if;

                    -- Update overflow flag
                    rho_overflow <= overflow_detected;
                end if;
            end if;
        end if;
    end process;

    ---------------------------------------------------------------------------
    -- Output Processing
    -- 1. Multiply input by rho
    -- 2. Store in 20-bit register
    -- 3. Output 8 MSBs for proper scaling
    ---------------------------------------------------------------------------
    output_processing: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                input_times_rho <= (others => '0');
                acc_output <= (others => '0');
                y_out <= (others => '0');
            else
                -- Multiply current input by correction factor
                -- Using x_in_current ensures time alignment with peak detection
                input_times_rho <= x_in_current * rho;

                -- Store in 20-bit register
                acc_output <= resize(input_times_rho, 20);

                -- Final output: take bits 19:12 from 20-bit result
                
                y_out <= std_logic_vector(acc_output(19 downto 12));
            end if;
        end if;
    end process;

    -- Connect debug signals
    dbg_peak_detected <= peak_detected;
    dbg_peak_value <= std_logic_vector(peak_value);
    dbg_rho <= std_logic_vector(rho);
    dbg_valid <= pipeline_valid;
    dbg_overflow <= rho_overflow;

end Behavioral;
