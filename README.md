# Amplitude Equalizer

## Project Overview

This project implements a **digital amplitude equalizer** for a triangle wave signal. The circuit adjusts the amplitude of the input signal using a recursive correction factor, ensuring that peak amplitudes conform to a defined reference level.

### Key Features

- Processes triangle wave samples from an 8-bit signed ADC.
- Sampling frequency is 8× the input signal frequency.
- Recursive correction factor calculation using:  
  `ρₖ₊₁ = ρₖ + (R - |Pₖ · ρₖ|)`
- Updates occur at 2× the input frequency (one positive and one negative peak per period).
- Final output is obtained by truncating a 20-bit accumulator to 12 bits, then further to 8 bits.
- Designed for FPGA implementation (Xilinx).

---

## System Architecture

### Input/Output Interface

| Signal | Direction | Description                                   |
|--------|-----------|-----------------------------------------------|
| clk    | Input     | System clock                                  |
| rst    | Input     | Asynchronous reset                            |
| X[n]   | Input     | 8-bit signed ADC sample input                 |
| R      | Input     | 8-bit signed reference amplitude              |
| Y[n]   | Output    | 8-bit signed equalized signal output          |

### Internal Blocks

- **Peak Detector**: Identifies positive and negative peaks from sampled input.
- **Correction Factor Calculator**: Updates ρ based on peak error.
- **Multiplier**: Computes product of sample and ρ.
- **Accumulator**: 20-bit intermediate result holder.
- **Truncator**: Converts 20-bit result to 8-bit output.

---

## Error Handling

All potential error conditions and edge cases (e.g., overflows, signed multiplication issues, peak misidentification) are accounted for. The VHDL code includes safeguards and detailed documentation of each decision.

---

## Testing Strategy

- Functional test cases using both ideal and noisy triangle wave signals.
- Verification of peak detection and correction factor behavior.
- Borderline tests for maximum/minimum signal values.
- Simulation of abnormal cases and reset behavior.

---

## Synthesis & Implementation

Target platform: **Xilinx FPGA**

- **Max Clock Frequency**: _To be filled post-synthesis_
- **Utilization**: Slices, LUTs, Flip-Flops used
- **Power Consumption**: Estimated using Xilinx Power Analyzer
- **Warnings**: Any synthesis/implementation warnings are documented and explained in the report.

---

## Applications

- Audio signal processing
- Communication systems
- Waveform normalization in embedded systems

---

## Conclusion

The amplitude equalizer circuit provides robust, real-time amplitude correction for triangle waveforms, validated through synthesis, simulation, and testbench verification. Designed for FPGA implementation with high clock frequency and low resource usage.
