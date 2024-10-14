import { useState } from 'react'
import './App.css'
import InputArea from './components/InputArea'

function App() {

  return (
    <>
      <h1>SAT Solver</h1>
      <div className="maincontainer">
        <InputArea/>
      </div>
    </>
  )
}

export default App