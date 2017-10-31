
makeEasyMIDIBackend' :: MIDIConfig -> IO (Backend' Word8)
makeEasyMIDIBackend' config = do
  midiBackend' <- makeMIDIBackend' config
  return $ \MkEnv{bpm, interval=(beat, _),clock, events} -> events
    & (>>=makeNoteEvents)
    & fmap (makeTimestamp bpm beat clock)
    & midiBackend'
  where
    makeNoteEvents :: Event Word8 -> [Event MIDIEvent.Data]
    makeNoteEvents MkEvent{interval=(start, dur), payload} =
      [ MkEvent {interval=(start, 0), payload=makeNoteOnData 0 payload}
      , MkEvent {interval=(start + dur, 0), payload=makeNoteOffData 0 payload}
      ]

rgb :: Float -> Float -> Float -> [OSC.OSC]
rgb r g b =
  [ OSC.OSC "/1/fader1" [OSC.OSC_F r]
  , OSC.OSC "/1/fader2" [OSC.OSC_F g]
  , OSC.OSC "/1/fader3" [OSC.OSC_F b]
  ]

tts :: Rational -> [Signal Word8 -> Signal Word8] -> Signal Word8 -> Signal Word8
tts x fs = tt x $ with switch fs

ttsf :: Rational -> [Word8] -> Signal Word8 -> Signal Word8
ttsf x fs = tt x $ with switch (fs & fmap  (\x -> fmap (+x)))

tc :: Rational -> [Signal a -> Signal a] -> Signal a -> Signal a
tc n fs = tt (1/n) $ with cat $ fmap (\f -> tt n f) fs

glisten :: Signal Word8 -> Signal Word8
glisten = fmap $ \x -> (x + ((x `mod` 4) * 12))

sigMod1 :: Signal Word8 -> Signal Word8
sigMod1 = let (>>) = (flip (.)) in do
  const (embed $ 60)
  staccato
  fast 4
  tt (2) $ do
    ttsf 2 [0, 3, 10, 12]
    ttsf (1/8) [17, 12, 10, 12]
    ttsf (1/2) [-12,-24]

sigMod2 :: Signal Word8 -> Signal Word8
sigMod2 = let (>>) = (flip (.)) in do
  const (embed $ 48)
  fmap (+24)
  fast 4
  -- tc (4) $
  --   [ ttsf 2 [-27, 0, -1, 7]
  --   , ttsf 2 [-27, -1, -8, 7]
  --   ]
  -- tts (1) [id, id, fmap (+(12)), id]
  -- tts (1/16) [id, fmap (+(-12))]
  -- tts (1/32) [id, fmap (+(-2))]


sigMod3 :: Signal Word8 -> Signal Word8
sigMod3 = let (>>) = (flip (.)) in do
  const (embed 60)
  tts 1 [ fmap (+(x)) | x <- [0, 4, 7, 11, 16, 18, 19, 21, 23]]
  fast 6
  tts (2) [ fmap (+(x)) | x <- [0, 0, 0, 0, 0,0, 12]]
  overlay $ do
    const (embed 36)
  --   s [fmap (+0), fmap (+2), fmap (+7), fmap (subtract 5)] & tt (1/16)
  -- fmap (+(-24))

sigMod4 :: Signal Word8 -> Signal Word8
sigMod4 = let (>>) = (flip (.)) in do
  const (embed 48)
  -- tts 1 [ fmap (+(x)) | x <- [0, 4, 7, 11, 16, 18, 19, 21, 23]]
  s [ fmap (+(x)) | x <- [0, 3, 10, 12, 19, 3]]
  fast 4
  tt 2 $ s [id, fmap (+12)]
  -- overlay $ do
  --   const (embed (120))

sigMod5 :: Signal Word8 -> Signal Word8
sigMod5 = let (>>) = (flip (.)) in do
  const (embed 48)
  s [fast 4]
  fmap (+24)
  tt 4 $ c [fmap (+3), fmap (+0)]
  tt 4 $ s [fmap (+0), fmap (+2), fmap (+0)]
  overlay $ do
    const $ embed 24
    -- tt (1/16) $ s [id, fmap (+(-2)), fmap (+(-4)), fmap (+(-7))]
    -- overlay $ do
    --   fmap (+12)
    -- overlay $ do
    --   fmap (+(7)) . shift 0.5
    -- s [fmap (+12), fmap (+24)]
    -- overlay $ s [tt (1/16) $ s [fmap (+10), fmap (+10), fmap (+14), fmap (+14)], fmap (+24)] . shift 0.5

sigMod6 :: Signal Word8 -> Signal Word8
sigMod6 = let (>>) = (flip (.)) in do
  const (embed 60)
  s [ fmap (+(x)) | x <- [0, 3, 10, 12, 19, 3]]
  s [fast 4]
  tt (1/4) $ s [id, fmap (+12)]
  tt (2/4) $ s [id, fmap (+12)]
  tt (4/4) $ s [id, fmap (+12)]
  glisten

