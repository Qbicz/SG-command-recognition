import math
import numpy as np
import pylab as plt
from scipy.fftpack import fft, ifft, fftfreq
from scipy.io import wavfile
from scipy.signal import butter, filtfilt, lfilter

filename = 'probka2.wav'
filename_out = 'probka2_filtered.wav'

[fs, frames] = wavfile.read(filename)
ts = 1 / fs
nyquist_f = fs / 2

# signal = (frames[:, 0] + frames[:, 1]) / 2
signal = frames[:, 0]  # left (?) channel
time = np.arange(0, len(signal) * ts, ts)

# we can use Hz in here when the filter is analog
low = 100
high = 10000

# actual filter
# b, a = butter(2, (low, high), 'bandpass', analog=True)
b, a = butter(2, (0.01, 0.2), 'bandpass')
bandpass = filtfilt(b, a, signal)
# bandpass = lfilter(b, a, signal)
bandpass = bandpass.astype('int16')

# save the results
wavfile.write(filename_out, fs, bandpass)

# Fourier transform of the original signal
freqs = fftfreq(signal.size, ts)
signal_f = fft(signal)

# Fourier of the bandpassed
freqs_band = fftfreq(bandpass.size, ts)
bandpass_f = fft(bandpass)

plt.subplot(221)
plt.plot(time, signal)

plt.subplot(222)
plt.plot(freqs, signal_f)

plt.subplot(223)
plt.plot(time, bandpass)

plt.subplot(224)
plt.plot(freqs_band, bandpass_f)

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
