{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.MikMod.Error where

import Foreign.C.Types

#include <mikmod.h>

data MikModError =
  MMErrOutOfMemory |
  MMErrDynamicLinking |
  MMErrSampleTooBig |
  MMErrOutOfHandles |
  MMErrUnknownWaveType |
  MMErrLoadingPattern |
  MMErrLoadingTrack |
  MMErrLoadingHeader |
  MMErrLoadingSampleinfo |
  MMErrNotAModule |
  MMErrNotAStream |
  MMErrMedSynthsamples |
  MMErrItpackInvalidData |
  MMErrDetectingDevice |
  MMErrInvalidDevice |
  MMErrInitializingMixer |
  MMErrOpeningAudio |
  MMErr8bitOnly |
  MMErr16bitOnly |
  MMErrStereoOnly |
  MMErrUlaw |
  MMErrNonBlock |
  MMErrAfAudioPort |
  MMErrAixConfigInit |
  MMErrAixConfigControl |
  MMErrAixConfigStart |
  MMErrGusSettings |
  MMErrGusReset |
  MMErrGusTimer |
  MMErrHpSetsamplesize |
  MMErrHpSetspeed |
  MMErrHpChannels |
  MMErrHpAudioOutput |
  MMErrHpAudioDesc |
  MMErrHpBuffersize |
  MMErrOssSetfragment |
  MMErrOssSetsamplesize |
  MMErrOssSetstereo |
  MMErrOssSetspeed |
  MMErrSgiSpeed |
  MMErrSgi16bit |
  MMErrSgi8bit |
  MMErrSgiStereo |
  MMErrSgiMono |
  MMErrSunInit |
  MMErrOs2Mixsetup |
  MMErrOs2Semaphore |
  MMErrOs2Timer |
  MMErrOs2Thread |
  MMErrDsPriority |
  MMErrDsBuffer |
  MMErrDsFormat |
  MMErrDsNotify |
  MMErrDsEvent |
  MMErrDsThread |
  MMErrDsUpdate |
  MMErrWinmmHandle |
  MMErrWinmmAllocated |
  MMErrWinmmDeviceid |
  MMErrWinmmFormat |
  MMErrWinmmUnknown |
  MMErrMacSpeed |
  MMErrMacStart |
  MMErrOsxUnknownDevice |
  MMErrOsxBadProperty |
  MMErrOsxUnsupportedFormat |
  MMErrOsxSetStereo |
  MMErrOsxBufferAlloc |
  MMErrOsxAddIoProc |
  MMErrOsxDeviceStart |
  MMErrOsxPthread |
  MMErrDoswssStartdma |
  MMErrDossbStartdma |
  MMErrNoFloat32 |
  MMErrOpenalCreatectx |
  MMErrOpenalCtxcurrent |
  MMErrOpenalGenbuffers |
  MMErrOpenalGensources |
  MMErrOpenalSource |
  MMErrOpenalQueuebuffers |
  MMErrOpenalUnqueuebuffers |
  MMErrOpenalBufferdata |
  MMErrOpenalGetsource |
  MMErrOpenalSourceplay |
  MMErrOpenalSourcestop |
  MMErrAlsaNoconfig |
  MMErrAlsaSetparams |
  MMErrAlsaSetformat |
  MMErrAlsaSetrate |
  MMErrAlsaSetchannels |
  MMErrAlsaBuffersize |
  MMErrAlsaPcmStart |
  MMErrAlsaPcmWrite |
  MMErrAlsaPcmRecover
    deriving (Eq, Ord, Show)

marshalMikModError :: MikModError -> CInt
marshalMikModError e = case e of
  MMErrOutOfMemory -> (#const MMERR_OUT_OF_MEMORY)
  MMErrDynamicLinking -> (#const MMERR_DYNAMIC_LINKING)
  MMErrSampleTooBig -> (#const MMERR_SAMPLE_TOO_BIG)
  MMErrOutOfHandles -> (#const MMERR_OUT_OF_HANDLES)
  MMErrUnknownWaveType -> (#const MMERR_UNKNOWN_WAVE_TYPE)
  MMErrLoadingPattern -> (#const MMERR_LOADING_PATTERN)
  MMErrLoadingTrack -> (#const MMERR_LOADING_TRACK)
  MMErrLoadingHeader -> (#const MMERR_LOADING_HEADER)
  MMErrLoadingSampleinfo -> (#const MMERR_LOADING_SAMPLEINFO)
  MMErrNotAModule -> (#const MMERR_NOT_A_MODULE)
  MMErrNotAStream -> (#const MMERR_NOT_A_STREAM)
  MMErrMedSynthsamples -> (#const MMERR_MED_SYNTHSAMPLES)
  MMErrItpackInvalidData -> (#const MMERR_ITPACK_INVALID_DATA)
  MMErrDetectingDevice -> (#const MMERR_DETECTING_DEVICE)
  MMErrInvalidDevice -> (#const MMERR_INVALID_DEVICE)
  MMErrInitializingMixer -> (#const MMERR_INITIALIZING_MIXER)
  MMErrOpeningAudio -> (#const MMERR_OPENING_AUDIO)
  MMErr8bitOnly -> (#const MMERR_8BIT_ONLY)
  MMErr16bitOnly -> (#const MMERR_16BIT_ONLY)
  MMErrStereoOnly -> (#const MMERR_STEREO_ONLY)
  MMErrUlaw -> (#const MMERR_ULAW)
  MMErrNonBlock -> (#const MMERR_NON_BLOCK)
  MMErrAfAudioPort -> (#const MMERR_AF_AUDIO_PORT)
  MMErrAixConfigInit -> (#const MMERR_AIX_CONFIG_INIT)
  MMErrAixConfigControl -> (#const MMERR_AIX_CONFIG_CONTROL)
  MMErrAixConfigStart -> (#const MMERR_AIX_CONFIG_START)
  MMErrGusSettings -> (#const MMERR_GUS_SETTINGS)
  MMErrGusReset -> (#const MMERR_GUS_RESET)
  MMErrGusTimer -> (#const MMERR_GUS_TIMER)
  MMErrHpSetsamplesize -> (#const MMERR_HP_SETSAMPLESIZE)
  MMErrHpSetspeed -> (#const MMERR_HP_SETSPEED)
  MMErrHpChannels -> (#const MMERR_HP_CHANNELS)
  MMErrHpAudioOutput -> (#const MMERR_HP_AUDIO_OUTPUT)
  MMErrHpAudioDesc -> (#const MMERR_HP_AUDIO_DESC)
  MMErrHpBuffersize -> (#const MMERR_HP_BUFFERSIZE)
  MMErrOssSetfragment -> (#const MMERR_OSS_SETFRAGMENT)
  MMErrOssSetsamplesize -> (#const MMERR_OSS_SETSAMPLESIZE)
  MMErrOssSetstereo -> (#const MMERR_OSS_SETSTEREO)
  MMErrOssSetspeed -> (#const MMERR_OSS_SETSPEED)
  MMErrSgiSpeed -> (#const MMERR_SGI_SPEED)
  MMErrSgi16bit -> (#const MMERR_SGI_16BIT)
  MMErrSgi8bit -> (#const MMERR_SGI_8BIT)
  MMErrSgiStereo -> (#const MMERR_SGI_STEREO)
  MMErrSgiMono -> (#const MMERR_SGI_MONO)
  MMErrSunInit -> (#const MMERR_SUN_INIT)
  MMErrOs2Mixsetup -> (#const MMERR_OS2_MIXSETUP)
  MMErrOs2Semaphore -> (#const MMERR_OS2_SEMAPHORE)
  MMErrOs2Timer -> (#const MMERR_OS2_TIMER)
  MMErrOs2Thread -> (#const MMERR_OS2_THREAD)
  MMErrDsPriority -> (#const MMERR_DS_PRIORITY)
  MMErrDsBuffer -> (#const MMERR_DS_BUFFER)
  MMErrDsFormat -> (#const MMERR_DS_FORMAT)
  MMErrDsNotify -> (#const MMERR_DS_NOTIFY)
  MMErrDsEvent -> (#const MMERR_DS_EVENT)
  MMErrDsThread -> (#const MMERR_DS_THREAD)
  MMErrDsUpdate -> (#const MMERR_DS_UPDATE)
  MMErrWinmmHandle -> (#const MMERR_WINMM_HANDLE)
  MMErrWinmmAllocated -> (#const MMERR_WINMM_ALLOCATED)
  MMErrWinmmDeviceid -> (#const MMERR_WINMM_DEVICEID)
  MMErrWinmmFormat -> (#const MMERR_WINMM_FORMAT)
  MMErrWinmmUnknown -> (#const MMERR_WINMM_UNKNOWN)
  MMErrMacSpeed -> (#const MMERR_MAC_SPEED)
  MMErrMacStart -> (#const MMERR_MAC_START)
  MMErrOsxUnknownDevice -> (#const MMERR_OSX_UNKNOWN_DEVICE)
  MMErrOsxBadProperty -> (#const MMERR_OSX_BAD_PROPERTY)
  MMErrOsxUnsupportedFormat -> (#const MMERR_OSX_UNSUPPORTED_FORMAT)
  MMErrOsxSetStereo -> (#const MMERR_OSX_SET_STEREO)
  MMErrOsxBufferAlloc -> (#const MMERR_OSX_BUFFER_ALLOC)
  MMErrOsxAddIoProc -> (#const MMERR_OSX_ADD_IO_PROC)
  MMErrOsxDeviceStart -> (#const MMERR_OSX_DEVICE_START)
  MMErrOsxPthread -> (#const MMERR_OSX_PTHREAD)
  MMErrDoswssStartdma -> (#const MMERR_DOSWSS_STARTDMA)
  MMErrDossbStartdma -> (#const MMERR_DOSSB_STARTDMA)
  MMErrNoFloat32 -> (#const MMERR_NO_FLOAT32)
  MMErrOpenalCreatectx -> (#const MMERR_OPENAL_CREATECTX)
  MMErrOpenalCtxcurrent -> (#const MMERR_OPENAL_CTXCURRENT)
  MMErrOpenalGenbuffers -> (#const MMERR_OPENAL_GENBUFFERS)
  MMErrOpenalGensources -> (#const MMERR_OPENAL_GENSOURCES)
  MMErrOpenalSource -> (#const MMERR_OPENAL_SOURCE)
  MMErrOpenalQueuebuffers -> (#const MMERR_OPENAL_QUEUEBUFFERS)
  MMErrOpenalUnqueuebuffers -> (#const MMERR_OPENAL_UNQUEUEBUFFERS)
  MMErrOpenalBufferdata -> (#const MMERR_OPENAL_BUFFERDATA)
  MMErrOpenalGetsource -> (#const MMERR_OPENAL_GETSOURCE)
  MMErrOpenalSourceplay -> (#const MMERR_OPENAL_SOURCEPLAY)
  MMErrOpenalSourcestop -> (#const MMERR_OPENAL_SOURCESTOP)
  MMErrAlsaNoconfig -> (#const MMERR_ALSA_NOCONFIG)
  MMErrAlsaSetparams -> (#const MMERR_ALSA_SETPARAMS)
  MMErrAlsaSetformat -> (#const MMERR_ALSA_SETFORMAT)
  MMErrAlsaSetrate -> (#const MMERR_ALSA_SETRATE)
  MMErrAlsaSetchannels -> (#const MMERR_ALSA_SETCHANNELS)
  MMErrAlsaBuffersize -> (#const MMERR_ALSA_BUFFERSIZE)
  MMErrAlsaPcmStart -> (#const MMERR_ALSA_PCM_START)
  MMErrAlsaPcmWrite -> (#const MMERR_ALSA_PCM_WRITE)
  MMErrAlsaPcmRecover -> (#const MMERR_ALSA_PCM_RECOVER)

unmarshalMikModError :: CInt -> MikModError
unmarshalMikModError code = case code of
  (#const MMERR_OUT_OF_MEMORY) -> MMErrOutOfMemory
  (#const MMERR_DYNAMIC_LINKING) -> MMErrDynamicLinking
  (#const MMERR_SAMPLE_TOO_BIG) -> MMErrSampleTooBig
  (#const MMERR_OUT_OF_HANDLES) -> MMErrOutOfHandles
  (#const MMERR_UNKNOWN_WAVE_TYPE) -> MMErrUnknownWaveType
  (#const MMERR_LOADING_PATTERN) -> MMErrLoadingPattern
  (#const MMERR_LOADING_TRACK) -> MMErrLoadingTrack
  (#const MMERR_LOADING_HEADER) -> MMErrLoadingHeader
  (#const MMERR_LOADING_SAMPLEINFO) -> MMErrLoadingSampleinfo
  (#const MMERR_NOT_A_MODULE) -> MMErrNotAModule
  (#const MMERR_NOT_A_STREAM) -> MMErrNotAStream
  (#const MMERR_MED_SYNTHSAMPLES) -> MMErrMedSynthsamples
  (#const MMERR_ITPACK_INVALID_DATA) -> MMErrItpackInvalidData
  (#const MMERR_DETECTING_DEVICE) -> MMErrDetectingDevice
  (#const MMERR_INVALID_DEVICE) -> MMErrInvalidDevice
  (#const MMERR_INITIALIZING_MIXER) -> MMErrInitializingMixer
  (#const MMERR_OPENING_AUDIO) -> MMErrOpeningAudio
  (#const MMERR_8BIT_ONLY) -> MMErr8bitOnly
  (#const MMERR_16BIT_ONLY) -> MMErr16bitOnly
  (#const MMERR_STEREO_ONLY) -> MMErrStereoOnly
  (#const MMERR_ULAW) -> MMErrUlaw
  (#const MMERR_NON_BLOCK) -> MMErrNonBlock
  (#const MMERR_AF_AUDIO_PORT) -> MMErrAfAudioPort
  (#const MMERR_AIX_CONFIG_INIT) -> MMErrAixConfigInit
  (#const MMERR_AIX_CONFIG_CONTROL) -> MMErrAixConfigControl
  (#const MMERR_AIX_CONFIG_START) -> MMErrAixConfigStart
  (#const MMERR_GUS_SETTINGS) -> MMErrGusSettings
  (#const MMERR_GUS_RESET) -> MMErrGusReset
  (#const MMERR_GUS_TIMER) -> MMErrGusTimer
  (#const MMERR_HP_SETSAMPLESIZE) -> MMErrHpSetsamplesize
  (#const MMERR_HP_SETSPEED) -> MMErrHpSetspeed
  (#const MMERR_HP_CHANNELS) -> MMErrHpChannels
  (#const MMERR_HP_AUDIO_OUTPUT) -> MMErrHpAudioOutput
  (#const MMERR_HP_AUDIO_DESC) -> MMErrHpAudioDesc
  (#const MMERR_HP_BUFFERSIZE) -> MMErrHpBuffersize
  (#const MMERR_OSS_SETFRAGMENT) -> MMErrOssSetfragment
  (#const MMERR_OSS_SETSAMPLESIZE) -> MMErrOssSetsamplesize
  (#const MMERR_OSS_SETSTEREO) -> MMErrOssSetstereo
  (#const MMERR_OSS_SETSPEED) -> MMErrOssSetspeed
  (#const MMERR_SGI_SPEED) -> MMErrSgiSpeed
  (#const MMERR_SGI_16BIT) -> MMErrSgi16bit
  (#const MMERR_SGI_8BIT) -> MMErrSgi8bit
  (#const MMERR_SGI_STEREO) -> MMErrSgiStereo
  (#const MMERR_SGI_MONO) -> MMErrSgiMono
  (#const MMERR_SUN_INIT) -> MMErrSunInit
  (#const MMERR_OS2_MIXSETUP) -> MMErrOs2Mixsetup
  (#const MMERR_OS2_SEMAPHORE) -> MMErrOs2Semaphore
  (#const MMERR_OS2_TIMER) -> MMErrOs2Timer
  (#const MMERR_OS2_THREAD) -> MMErrOs2Thread
  (#const MMERR_DS_PRIORITY) -> MMErrDsPriority
  (#const MMERR_DS_BUFFER) -> MMErrDsBuffer
  (#const MMERR_DS_FORMAT) -> MMErrDsFormat
  (#const MMERR_DS_NOTIFY) -> MMErrDsNotify
  (#const MMERR_DS_EVENT) -> MMErrDsEvent
  (#const MMERR_DS_THREAD) -> MMErrDsThread
  (#const MMERR_DS_UPDATE) -> MMErrDsUpdate
  (#const MMERR_WINMM_HANDLE) -> MMErrWinmmHandle
  (#const MMERR_WINMM_ALLOCATED) -> MMErrWinmmAllocated
  (#const MMERR_WINMM_DEVICEID) -> MMErrWinmmDeviceid
  (#const MMERR_WINMM_FORMAT) -> MMErrWinmmFormat
  (#const MMERR_WINMM_UNKNOWN) -> MMErrWinmmUnknown
  (#const MMERR_MAC_SPEED) -> MMErrMacSpeed
  (#const MMERR_MAC_START) -> MMErrMacStart
  (#const MMERR_OSX_UNKNOWN_DEVICE) -> MMErrOsxUnknownDevice
  (#const MMERR_OSX_BAD_PROPERTY) -> MMErrOsxBadProperty
  (#const MMERR_OSX_UNSUPPORTED_FORMAT) -> MMErrOsxUnsupportedFormat
  (#const MMERR_OSX_SET_STEREO) -> MMErrOsxSetStereo
  (#const MMERR_OSX_BUFFER_ALLOC) -> MMErrOsxBufferAlloc
  (#const MMERR_OSX_ADD_IO_PROC) -> MMErrOsxAddIoProc
  (#const MMERR_OSX_DEVICE_START) -> MMErrOsxDeviceStart
  (#const MMERR_OSX_PTHREAD) -> MMErrOsxPthread
  (#const MMERR_DOSWSS_STARTDMA) -> MMErrDoswssStartdma
  (#const MMERR_DOSSB_STARTDMA) -> MMErrDossbStartdma
  (#const MMERR_NO_FLOAT32) -> MMErrNoFloat32
  (#const MMERR_OPENAL_CREATECTX) -> MMErrOpenalCreatectx
  (#const MMERR_OPENAL_CTXCURRENT) -> MMErrOpenalCtxcurrent
  (#const MMERR_OPENAL_GENBUFFERS) -> MMErrOpenalGenbuffers
  (#const MMERR_OPENAL_GENSOURCES) -> MMErrOpenalGensources
  (#const MMERR_OPENAL_SOURCE) -> MMErrOpenalSource
  (#const MMERR_OPENAL_QUEUEBUFFERS) -> MMErrOpenalQueuebuffers
  (#const MMERR_OPENAL_UNQUEUEBUFFERS) -> MMErrOpenalUnqueuebuffers
  (#const MMERR_OPENAL_BUFFERDATA) -> MMErrOpenalBufferdata
  (#const MMERR_OPENAL_GETSOURCE) -> MMErrOpenalGetsource
  (#const MMERR_OPENAL_SOURCEPLAY) -> MMErrOpenalSourceplay
  (#const MMERR_OPENAL_SOURCESTOP) -> MMErrOpenalSourcestop
  (#const MMERR_ALSA_NOCONFIG) -> MMErrAlsaNoconfig
  (#const MMERR_ALSA_SETPARAMS) -> MMErrAlsaSetparams
  (#const MMERR_ALSA_SETFORMAT) -> MMErrAlsaSetformat
  (#const MMERR_ALSA_SETRATE) -> MMErrAlsaSetrate
  (#const MMERR_ALSA_SETCHANNELS) -> MMErrAlsaSetchannels
  (#const MMERR_ALSA_BUFFERSIZE) -> MMErrAlsaBuffersize
  (#const MMERR_ALSA_PCM_START) -> MMErrAlsaPcmStart
  (#const MMERR_ALSA_PCM_WRITE) -> MMErrAlsaPcmWrite
  (#const MMERR_ALSA_PCM_RECOVER) -> MMErrAlsaPcmRecover
