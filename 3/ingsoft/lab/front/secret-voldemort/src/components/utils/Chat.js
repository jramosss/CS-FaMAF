import React, { useState, useRef, useEffect, Component }  from "react";
import { sendRequest } from "../../services/request";

// TODO: Mayor ancho, autoscrolear hacie abajo


// props: messages, room_name, token
export const Chat = props => {
  const [text, setText] = useState('')

  useEffect(()=>{
    let chat = document.getElementById('chat')
    chat.scrollTo(0,chat.scrollHeight)
  })
  
  const sendMessage = e => {
    e.preventDefault()
    const method = 'PUT'
    const path = `http://localhost:8000/${props.room_name}/chat`
    const body = {msg: text}
    const header = {
      Accept: "application/json",
      Authorization: 'Bearer ' + props.token,
      "Content-Type": "application/json"
    }
    sendRequest(method, header, body, path).then(async response => {
      const data = await response.json()
      if (response.ok) {
        console.log('Message was delivered succesfully')
      } else {
        console.log(data.detail)
      }
    }).catch(error => {
      console.log(error.detail);
    }) 
  }

  return(
    <div class='column is-12'>
      <div id='chat' class='chat py-0 my-0'>
        {
        }
        <ul>
          {props.messages.map(m =>
            <li>
              <p><strong>{m.split(':')[0]}:</strong> {m.split(':')[1]}</p>  
            </li>
          )}
        </ul>
      </div>
      {
        (props.name == '' || (props.phase !== 3 && props.phase !== 4) || (props.name !== props.minister && props.name !== props.director)) ? 
          <input class='chat-input' type='text' placeholder='write a message - "Enter" to send' value={text} onChange={e => setText(e.target.value)} onKeyDown={e => {
            if(e.key == "Enter") {
              sendMessage(e)
              setText('')
            }
          }}/>
        : <input class='chat-input' type='text' placeholder="Legislation phase, silent please" value={text} onChange={e => setText(e.target.value)}/>
      }
    </div>
  )
}