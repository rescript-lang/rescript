open React
let logo:string = %raw("require('./assets/logo.png')")

@react.component
let make = () => {
  useEffect0(_ => {
    AudioPlayer.play()->ignore
    None
  })
  
  // Template
  <div className="h-screen flex justify-between  flex-col p-8">
    <div className=""> {"Other stuff here"->string} </div>
    <div id="controls" className="flex flex-col items-center">
      <SignaturePicker />
      <div className="h-8" />
      <FatSlider
        meterSuffix={<span className="text-lg text-gray-400"> {" BPM"->string} </span>}
      />
      <div className="h-8" />
    </div>
  </div>
}
