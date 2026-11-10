# Audio for voice specs: a streamed piece is 16-bit PCM WAV, as the browser recorder sends it.
module VoiceAudio
  def voice_piece(seconds, rate: 16000, channels: 1)
    pcm = ([0, 1000, 0, -1000] * (rate * seconds * channels / 4).to_i).pack('s<*')
    header = ['RIFF', 36 + pcm.bytesize, 'WAVE', 'fmt ', 16, 1, channels, rate, rate * 2 * channels, 2 * channels, 16, 'data', pcm.bytesize].pack('a4Va4a4VvvVVvva4V')
    [header + pcm].pack('m0')
  end
end
