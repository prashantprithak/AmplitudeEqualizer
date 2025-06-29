library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use STD.TEXTIO.ALL;
use IEEE.STD_LOGIC_TEXTIO.ALL;

entity AmplitudeEqualizer_TB is
end AmplitudeEqualizer_TB;

architecture Behavioral of AmplitudeEqualizer_TB is
    -- Component declaration for DUT
    component AmplitudeEqualizer is
        Port (
            clk                 : in  STD_LOGIC;
            rst                 : in  STD_LOGIC;
            x_in                : in  STD_LOGIC_VECTOR(7 downto 0);
            ref                 : in  STD_LOGIC_VECTOR(15 downto 0);
            y_out               : out STD_LOGIC_VECTOR(7 downto 0);
            dbg_peak_detected   : out STD_LOGIC;
            dbg_peak_value      : out STD_LOGIC_VECTOR(7 downto 0);
            dbg_rho             : out STD_LOGIC_VECTOR(7 downto 0);
            dbg_valid           : out STD_LOGIC;
            dbg_overflow        : out STD_LOGIC
        );
    end component;
    
    -- Define array type for test data
    type int_array is array (natural range <>) of integer;
    
    -- Define record for expected peak positions
    type peak_info is record
        position : integer;
        value : integer;
        is_positive : boolean;
    end record;
    
    type peak_array is array (natural range <>) of peak_info;
    
    -- Test case data arrays
    -- Test case 1: 5 cycles of triangle wave
    constant TEST_DATA_1 : int_array := (
        0, 64, 127, 96, 0, -96, -127, -60,
        25, 63, 125, 95, -2, -95, -126, -40,
        0, 65, 127, 97, 2, -98, -128, 0,
        45, 66, 126, 94, -1, -97, -125, -56,
        0, 62, 127, 98, 0, -94, -127, -2
    );
    
    -- Expected peak positions and values for test case 1
    constant PEAKS_1 : peak_array := (
        (2, 127, true),    -- Position 2, value 127: Positive peak
        (6, -127, false),  -- Position 6, value -127: Negative peak
        (10, 125, true),   -- Position 10, value 125: Positive peak
        (14, -126, false), -- Position 14, value -126: Negative peak
        (18, 127, true),   -- Position 18, value 127: Positive peak
        (22, -128, false), -- Position 22, value -128: Negative peak
        (26, 126, true),   -- Position 26, value 126: Positive peak
        (30, -125, false), -- Position 30, value -125: Negative peak
        (34, 127, true),   -- Position 34, value 127: Positive peak
        (38, -127, false)  -- Position 38, value -127: Negative peak
    );
    
    -- Other test case constants remain unchanged
    constant TEST_DATA_2 : int_array := (
        0, 16, 31, 24, 0, -24, -31, -15,
        6, 15, 31, 23, 0, -23, -31, -10,
        0, 16, 31, 24, 0, -24, -32, 0,
        11, 16, 31, 23, 0, -24, -31, -14,
        0, 15, 31, 24, 0, -23, -31, 0
    );
    
    constant PEAKS_2 : peak_array := (
        (2, 31, true),     -- Position 2, value 31: Positive peak
        (6, -31, false),   -- Position 6, value -31: Negative peak
        (10, 31, true),    -- Position 10, value 31: Positive peak
        (14, -31, false),  -- Position 14, value -31: Negative peak
        (18, 31, true),    -- Position 18, value 31: Positive peak
        (22, -32, false),  -- Position 22, value -32: Negative peak
        (26, 31, true),    -- Position 26, value 31: Positive peak
        (30, -31, false),  -- Position 30, value -31: Negative peak
        (34, 31, true),    -- Position 34, value 31: Positive peak
        (38, -31, false)   -- Position 38, value -31: Negative peak
    );
    
    constant TEST_DATA_3 : int_array := (
        0, 120, 127, 120, 0, -120, -127, -120,
        0, 120, 127, 120, 0, -120, -127, -120,
        0, 120, 127, 120, 0, -120, -127, -120,
        0, 120, 127, 120, 0, -120, -127, -120,
        0, 120, 127, 120, 0, -120, -127, -120
    );
    
    constant TEST_DATA_4 : int_array := (
        0, 70, 100, 60, 0, -50, -90, -40,
        20, 65, 95, 55, -5, -70, -100, -60,
        0, 60, 110, 70, 10, -80, -105, -30,
        15, 75, 115, 60, -10, -85, -110, -45,
        0, 80, 120, 50, 0, -90, -115, -40
    );
    
    constant TEST_DATA_5 : int_array := (
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
    );
    
    constant TEST_DATA_6 : int_array := (
        0, 64, 127, 96, 0, -96, -127, -60,
        0, 10, 20, 15, 0, -15, -20, -10,  -- Sudden drop in amplitude
        0, 120, 127, 120, 0, -120, -127, -120,  -- Sudden increase in amplitude
        0, 64, 127, 96, 0, -96, -127, -60,
        0, 64, 127, 96, 0, -96, -127, -60
    );
    
    constant TEST_DATA_7 : int_array := (
        0, 64, 127, 96, 0, -96, -128, -60,  -- Note: -128 is minimum 8-bit signed value
        0, 64, 127, 96, 0, -96, -128, -60,
        0, 64, 127, 96, 0, -96, -128, -60,
        0, 64, 127, 96, 0, -96, -128, -60,
        0, 64, 127, 96, 0, -96, -128, -60
    );
    
    -- Clock period definition
    constant CLK_PERIOD : time := 10 ns;
    
    -- Testbench signals
    signal clk                 : std_logic := '0';
    signal rst                 : std_logic := '1';
    signal x_in                : std_logic_vector(7 downto 0) := (others => '0');
    signal ref                 : std_logic_vector(15 downto 0) := std_logic_vector(to_unsigned(1023, 16)); -- Reference (2^10-1)
    signal y_out               : std_logic_vector(7 downto 0);
    signal dbg_peak_detected   : std_logic;
    signal dbg_peak_value      : std_logic_vector(7 downto 0);
    signal dbg_rho             : std_logic_vector(7 downto 0);
    signal dbg_valid           : std_logic;
    signal dbg_overflow        : std_logic;
    
    signal adjacent_left      : signed(7 downto 0) := (others => '0');
    signal adjacent_center    : signed(7 downto 0) := (others => '0');
    signal adjacent_right     : signed(7 downto 0) := (others => '0');
    
    -- Test control signals
    signal test_case           : integer := 0;
    signal sim_done            : boolean := false;
    
    -- Sample history buffer (increased to 5 samples to capture more history)
    signal sample_history      : int_array(0 to 4) := (0, 0, 0, 0, 0);  -- Store last 5 samples
    
    -- Performance metrics
    type metric_record is record
        initial_rho        : integer;
        final_rho          : integer;
        num_peaks_detected : integer;
        max_output_value   : integer;
        min_output_value   : integer;
        convergence_cycles : integer;  -- Number of cycles to reach stable rho
        num_overflows      : integer;  -- Number of rho calculation overflows
    end record;
    
    signal metrics : metric_record := (0, 0, 0, 0, 0, 0, 0);
    
begin
    -- Instantiate the Unit Under Test (UUT)
    uut: AmplitudeEqualizer
    port map (
        clk               => clk,
        rst               => rst,
        x_in              => x_in,
        ref               => ref,
        y_out             => y_out,
        dbg_peak_detected => dbg_peak_detected,
        dbg_peak_value    => dbg_peak_value,
        dbg_rho           => dbg_rho,
        dbg_valid         => dbg_valid,
        dbg_overflow      => dbg_overflow
    );
    
    -- Clock process
    --Creates a clock process that generates a continuous clock signal until sim_done becomes true
    clk_process: process
    begin
        while not sim_done loop
            clk <= '0';
            wait for CLK_PERIOD/2;
            clk <= '1';
            wait for CLK_PERIOD/2;
        end loop;
        wait;
    end process;
    
    -- Assertion process for peak detection verification
    peak_assertion: process(clk)
        variable expected_peak_index : integer := 0;
        variable peak_position : integer := 0;
        variable current_peak_array : peak_array(0 to 9) := PEAKS_1;
    begin
        if rising_edge(clk) then
            if rst = '0' and dbg_valid = '1' then
                -- Track peak detection and verify against expected positions
                if dbg_peak_detected = '1' then
                    -- Check if this peak was expected
                    if test_case = 1 then
                        current_peak_array := PEAKS_1;
                    elsif test_case = 2 then
                        current_peak_array := PEAKS_2;
                    end if;
                    
                    -- Only check for test cases 1-2 which have defined expected peaks
                    if test_case >= 1 and test_case <= 2 and expected_peak_index < 10 then
                        peak_position := current_peak_array(expected_peak_index).position;
                        
                        -- Check if the peak value is correct (or close)
                        assert abs(to_integer(signed(dbg_peak_value)) - current_peak_array(expected_peak_index).value) <= 5
                            report "Unexpected peak value. Expected value around " & 
                                   integer'image(current_peak_array(expected_peak_index).value) & 
                                   ", detected value is " & integer'image(to_integer(signed(dbg_peak_value)))
                            severity warning;
                            
                        -- Increment expected peak index
                        expected_peak_index := expected_peak_index + 1;
                    end if;
                end if;
            end if;
        end if;
    end process;
    
    -- Metrics collection process
    metrics_collection: process(clk)
        variable current_output_value : integer := 0;
        variable prev_rho_value : integer := 0;
        variable stable_count : integer := 0;
        variable rho_stable : boolean := false;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                -- Reset metrics
                metrics.initial_rho <= 0;
                metrics.final_rho <= 0;
                metrics.num_peaks_detected <= 0;
                metrics.max_output_value <= -128;  -- Minimum possible 8-bit signed value
                metrics.min_output_value <= 127;   -- Maximum possible 8-bit signed value
                metrics.convergence_cycles <= 0;
                metrics.num_overflows <= 0;
                
                -- Reset local variables
                prev_rho_value := 0;
                stable_count := 0;
                rho_stable := false;
            elsif dbg_valid = '1' then
                -- Record initial rho value
                if metrics.initial_rho = 0 then
                    metrics.initial_rho <= to_integer(signed(dbg_rho));
                end if;
                
                -- Track peak detections
                if dbg_peak_detected = '1' then
                    metrics.num_peaks_detected <= metrics.num_peaks_detected + 1;
                end if;
                
                -- Track output value range
                current_output_value := to_integer(signed(y_out));
                if current_output_value > metrics.max_output_value then
                    metrics.max_output_value <= current_output_value;
                end if;
                if current_output_value < metrics.min_output_value then
                    metrics.min_output_value <= current_output_value;
                end if;
                
                -- Track rho overflows
                if dbg_overflow = '1' then
                    metrics.num_overflows <= metrics.num_overflows + 1;
                end if;
                
                -- Update final rho value 
                metrics.final_rho <= to_integer(signed(dbg_rho));
                
                -- Track convergence
                if not rho_stable then
                    if prev_rho_value = to_integer(signed(dbg_rho)) then
                        stable_count := stable_count + 1;
                        if stable_count >= 16 then -- Consider rho stable if unchanged for 16 cycles
                            rho_stable := true;
                            metrics.convergence_cycles <= metrics.convergence_cycles;
                        end if;
                    else
                        prev_rho_value := to_integer(signed(dbg_rho));
                        stable_count := 0;
                        metrics.convergence_cycles <= metrics.convergence_cycles + 1;
                    end if;
                end if;
            end if;
        end if;
    end process;
    
    -- Output convergence check
    output_check: process(clk)
        variable prev_peak_value : integer := 0;
        variable peak_diff : integer := 0;
        variable target_reached : boolean := false;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                prev_peak_value := 0;
                target_reached := false;
            elsif dbg_valid = '1' and dbg_peak_detected = '1' then
                -- Check if peak output is approaching reference value
                if not target_reached then
                    if abs(to_integer(signed(y_out))) > 0 then -- Avoid division by zero
                        -- Calculate how close we are to target (1023)
                        peak_diff := abs(1023 - abs(to_integer(signed(y_out))));
                        
                        -- If close enough to target (within 5%), flag as converged
                        if peak_diff < (1023 * 5 / 100) then
                            target_reached := true;
                            report "Output converged to within 5% of reference value after " & 
                                   integer'image(metrics.num_peaks_detected) & " peaks"
                            severity note;
                        end if;
                    end if;
                end if;
            end if;
        end if;
    end process;
    
    -- Stimulus process
    stim_proc: process
        variable test_data      : int_array(0 to 39);
        variable test_name      : string(1 to 40);
        variable sample_idx     : integer := 0;  -- Track actual sample index
        variable peak_type_str : string(1 to 8) := "        ";
        
        -- Signal to track initial rho
        variable initial_rho_tracked : boolean := false;
        
        procedure run_test_case(case_num : integer; test_data : int_array; name : string) is
        begin
            -- Set test case number for assertions
            test_case <= case_num;
            
            -- Reset sample history buffer and index
            sample_history <= (0, 0, 0, 0, 0);
            sample_idx := 0;
            initial_rho_tracked := false;
            
            -- Print test case header to console
            report "-----------------------------------------------------";
            report "Starting Test Case " & integer'image(case_num) & ": " & name;
            report "-----------------------------------------------------";
            
            -- Reset the DUT
            rst <= '1';
            wait for CLK_PERIOD * 5;
            rst <= '0';
            wait for CLK_PERIOD * 2;
            
            -- Apply the test data
            for i in test_data'range loop
                -- Update sample history buffer by shifting
                sample_history(0 to 3) <= sample_history(1 to 4);
                sample_history(4) <= test_data(i);
                
                -- Convert integer to signed 8-bit
                x_in <= std_logic_vector(to_signed(test_data(i), 8));
                
                -- Wait for clock edge
                wait for CLK_PERIOD;
                
                -- For the first sample, track initial rho (only once)
                if not initial_rho_tracked then
                    -- Track initial rho value
                    report "Sample 0 (index 0): Input = " & integer'image(test_data(i)) & 
                          ", Initial Rho = 1";
                    initial_rho_tracked := true;
                else
                    -- Log data to console - modified to always report regardless of dbg_valid
                    if dbg_peak_detected = '1' and dbg_valid = '1' then
                        if to_integer(signed(dbg_peak_value)) >= 0 then
                            peak_type_str := "POSITIVE";
                        else
                            peak_type_str := "NEGATIVE";
                        end if;
                        
                        -- Store adjacent samples
                        adjacent_left <= to_signed(sample_history(1), 8);
                        adjacent_center <= to_signed(sample_history(2), 8); 
                        adjacent_right <= to_signed(sample_history(3), 8);
                        
                        report "Sample " & integer'image(i) & " (index " & integer'image(sample_idx) & 
                              "): Input = " & integer'image(test_data(i)) & 
                              ", Output = " & integer'image(to_integer(signed(y_out))) &
                              ", Rho = " & integer'image(to_integer(signed(dbg_rho))) &
                              ", Peak Value = " & integer'image(to_integer(signed(dbg_peak_value))) &
                              ", Actual Peak = " & integer'image(sample_history(2)) &
                              ", Adjacent Samples = [" & integer'image(sample_history(1)) & ", " & 
                                   integer'image(sample_history(2)) & ", " & integer'image(sample_history(3)) & "]" &
                              " at Sample " & integer'image(i-2) &  " --> " & peak_type_str & " PEAK DETECTED!";
                    else
                        -- Reset signals when no peak detected
                        adjacent_left <= (others => '0');
                        adjacent_center <= (others => '0');
                        adjacent_right <= (others => '0');
                        
                        -- Always report sample information regardless of dbg_valid
                        report "Sample " & integer'image(i) & " (index " & integer'image(sample_idx) & 
                              "): Input = " & integer'image(test_data(i)) & 
                              ", Output = " & integer'image(to_integer(signed(y_out))) &
                              ", Rho = " & integer'image(to_integer(signed(dbg_rho)));
                    end if;
                end if;
                
                -- Increment actual sample index
                sample_idx := sample_idx + 1;
            end loop;
            
            -- Allow some time for the system to stabilize
            wait for CLK_PERIOD * 5;
            
            -- Summary for this test case
            report "-----------------------------------------------------";
            report "Test Case " & integer'image(case_num) & " Summary:";
            report "Final rho value: " & integer'image(metrics.final_rho);
            report "Peaks detected: " & integer'image(metrics.num_peaks_detected);
            report "Output range: [" & integer'image(metrics.min_output_value) & ", " & 
                   integer'image(metrics.max_output_value) & "]";
            report "Rho convergence cycles: " & integer'image(metrics.convergence_cycles);
            report "Rho overflows: " & integer'image(metrics.num_overflows);
            report "-----------------------------------------------------";
            
            -- Reset metrics for next test
            rst <= '1';
            wait for CLK_PERIOD;
            rst <= '0';
            wait for CLK_PERIOD;
        end procedure;
        
    begin
        -- Write header to console
        report "AMPLITUDE EQUALIZER TESTBENCH RESULTS";
        report "Simulation Test Run";
        report "Reference value (R): 1023";
        
        -- Run Test Case 1
        run_test_case(1, TEST_DATA_1, "Normal Triangle Wave               ");
        
        -- Run Test Case 2
        run_test_case(2, TEST_DATA_2, "Small Amplitude Wave (amplification)");
        
        -- Run Test Case 3
        run_test_case(3, TEST_DATA_3, "Large Amplitude Wave (attenuation)  ");
        
        -- Run Test Case 4
        run_test_case(4, TEST_DATA_4, "Irregular Triangle Wave             ");
        
        -- Run Test Case 5
        run_test_case(5, TEST_DATA_5, "Zero Input (Borderline Case)        ");
        
        -- Run Test Case 6
        run_test_case(6, TEST_DATA_6, "Abrupt Amplitude Change             ");
        
        -- Run Test Case 7
        run_test_case(7, TEST_DATA_7, "Extreme Input Values                ");
        
        -- Signal simulation end
        report "All tests completed!";
        sim_done <= true;
        
        -- End simulation
        wait;
    end process;

end Behavioral;
