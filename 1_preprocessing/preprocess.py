import math
import numpy as np
import pylab as plt
from scipy.fftpack import fft, fftfreq
from scipy.io import wavfile
from scipy.signal import butter, filtfilt, freqs

filename = 'probka1.wav'
filename_out = 'probka1_filtered.wav'

[fs, frames] = wavfile.read(filename)
ts = 1 / fs
nyquist_f = fs / 2

# signal = (frames[:, 0] + frames[:, 1]) / 2  # mean value of both channels
signal = frames[:, 0]  # left (?) channel
time = np.arange(0, len(signal) * ts, ts)

# signal = signal[(time < 0.5)]
# time = time[(time < 0.5)]

# values in fraction of nyquist_f
low = 0.1
high = 0.20

# actual filter
b, a = butter(2, (low, high), 'bandpass')
w, h = freqs(b, a)  # frequency response of the filter
bandpass = filtfilt(b, a, signal)  # apply filter to the signal
bandpass *= 2  # GAIN SOME POWER
bandpass = bandpass.astype('int16')  # to save the results in correct format

# save the results
wavfile.write(filename_out, fs, bandpass)

# Fourier transform of the original signal
frequencies = fftfreq(signal.size, ts)
frequencies = frequencies[range(int(len(frequencies) / 2))]
signal_f = fft(signal) / len(signal)
signal_f = signal_f[range(int(len(signal_f) / 2))]  # only for positive f.
# signal_f = signal_f[(frequencies < 1000)]
# frequencies = frequencies[(frequencies < 1000)]

# Fourier of the bandpassed
freqs_band = fftfreq(bandpass.size, ts)
freqs_band = freqs_band[range(int(len(freqs_band) / 2))]
bandpass_f = fft(bandpass) / len(bandpass)
bandpass_f = bandpass_f[range(int(len(bandpass_f) / 2))]
# bandpass_f = bandpass_f[(freqs_band < 1000)]
# freqs_band = freqs_band[(freqs_band < 1000)]

plt.subplot(2, 3, 1)
plt.plot(time, signal)
plt.xlabel('Time [s]')
plt.ylabel('Amplitude')
plt.grid()

plt.subplot(2, 3, 4)
plt.plot(frequencies, np.abs(signal_f), 'r')
plt.xlabel('Freq [Hz]')
plt.ylabel('|Y(freq)|')
plt.grid()

plt.subplot(2, 3, 2)
plt.plot(time, bandpass)
plt.xlabel('Time [s]')
plt.ylabel('Amplitude')
plt.grid()

plt.subplot(2, 3, 5)
plt.plot(freqs_band, np.abs(bandpass_f), 'r')
plt.xlabel('Freq [Hz]')
plt.ylabel('|Y(freq)|')
plt.grid()

plt.subplot(2, 3, 3)
plt.semilogx(w, 20 * np.log10(abs(h)))
plt.title('Butterworth filter frequency response')
plt.xlabel('Freq [rad/s]')
plt.ylabel('Amplitude [dB]')
plt.margins(0, 0.1)
plt.grid(which='both', axis='both')

plt.show()

# overlapping frames
frame_length = 20e-3  # in seconds, ie. 20ms
samples_per_frame = math.ceil(frame_length / ts)
step = samples_per_frame // 2  # integer division, Python 3.x only
frames = []
for i in range(0, len(signal), step):
    frame = signal[i:(frame_length + i)]
    frames.append(frame)

# save those frames somewhere?
