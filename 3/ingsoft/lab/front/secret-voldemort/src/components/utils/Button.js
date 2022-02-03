import React, { useState,  useEffect } from 'react';
import {Route} from 'react-router-dom'
import Cookies from 'js-cookie';

/* This component allows you to generate a custom redirectable button. */
export const Button = (props) => {

  const [path, setPath] = useState('')
  const [text, setText] = useState('')
  const [username, setUsername] = useState('')
  const [logout, setLogout] = useState('')
  const [style, setStyle] = useState('')
  const [namebutton, setNameButton] = useState('')
  const [id, setId] = useState('')

  useEffect(() => {
    setPath(props.path)
    setText(props.text)
    setUsername(props.username)
    setLogout(props.logout)
    setStyle(props.style)
    setNameButton(props.type)
    setId(props.id)
  }, [props])

    if (logout) {
      Cookies.remove(username)
    } else {
      return (
        <Route render={({history}) => (
          <button
            id={id}
            type={namebutton}
            class={style}
            onClick={() => { history.push(path) }}
          >
          {text}
          </button>
        )} />
      )
    }
}
