import React, { useState, useEffect, useContext } from 'react';
import { userContext } from '../../user-context';
import Cookies from 'js-cookie';
import { Redirect } from 'react-router-dom';
import { Button } from '../utils/Button';
import { sendRequest } from '../../services/request';
import { SetCookies } from '../utils/SetCookies';
import App from '../../App';

/* 
* Here are those functions related to the user's profile. 
*/


/* This function allows you to update the nickname, requiring that the new nickname meets the established requirements. */

export function ChangeNickname() {

  const context = useContext(userContext);
  const [nickname, setNickname] = useState('');
  const [nameValidation, setNameValidation] = useState('')
  const [badResponseDetail, setBadResponseDetail] = useState('')
  const [goodResponseDetail, setGoodResponseDetail] = useState('')
  const [fieldValidation, setFieldValidations] = useState('')

  useEffect(() => {
    if(context.username === '') {
      const cookie = Cookies.getJSON("user");
      if(cookie !== undefined) {
          context.setUsername(cookie.username);
          context.setEmail(cookie.email);
          context.setToken(cookie.token);
          context.setIcon(cookie.icon);
      }
  }
  }, [context])

  const validateFields = (e) => {
    const field_name = e.target.name
    const field_value = e.target.value
    
    if (field_value.length === 0) {
      return setNameValidation(`${field_name} is required`)
    }
    
    if (field_value && (field_value.length < 3 || field_value.length > 15)) {
      return setNameValidation('Nickname must contain between 3 and 15 characters')
    }

    //If everything is correct I don't show a message.

    return setNameValidation('')
    
  }
  
  const handleNickname = (e) => {
    setBadResponseDetail('')
    setFieldValidations('')
    setNameValidation('')
    setGoodResponseDetail('')
    setNickname(e.target.value)
  }
  
  const handleSubmit = (e) => {

    e.preventDefault()
  
    const headers = {
      Accept: "application/json",
      Authorization: "Bearer " + context.token,
      "Content-Type": "application/json"
    }

    const keys = {username: nickname}

    /* If there is no error in the name validation and there is indeed a name ... */

    if(nameValidation === '' && nickname.length !== 0) {
      sendRequest("PUT", headers, keys, "http://127.0.0.1:8000/users/change_username")
        .then(async response => {
          const data = await response.json();

          if(!response.ok) {
            return setBadResponseDetail(data.detail)
          } else {
            /* If the name was changed correctly I must refresh the token */
            const headers2 = {
              Accept: "application/json",
              Authorization: "Bearer " + context.token
            }
            sendRequest("PUT", headers2, {}, "http://127.0.0.1:8000/users/refresh")
              .then(async response2 => {
                const data2 = await response2.json();

                if(!response2.ok) {
                  return setBadResponseDetail(data2.detail)
                } else {
                  
                  context.setUsername(nickname)
                  context.setToken(data2.access_token)
                  console.log(context.token)
                  Cookies.remove("user")
                  SetCookies("user", nickname, data2.access_token, context.email, context.icon)
                  return setGoodResponseDetail(data.message)
                }
              })
          }
        })
    } else {
      if (nickname.length === 0) {
        setFieldValidations('Please enter a nickname to update it.')
      }
    }

  }

  return(
    <div>
      { (Cookies.get("user") !== undefined) ?
          <section class='room-bg'>
          <div class='container has-text-centered mt-6'>
            <form onSubmit={handleSubmit}>
              <div class="field">
                  <label class='room-label'>New nickname</label>
                  <div>
                    <input type="text" name="Nickname" id="nameUser" 
                      value={nickname} onChange={handleNickname} onBlur={validateFields}/> 
                  </div>
                  <div id='nameValidation' class="help is-danger">{nameValidation}</div>
              </div>
              <input class='room-button mx-2 my-2' id='Update' name='Update' type='submit' value='Update'/>
              <Button style='room-button mx-2 my-2' id='Home' path='/home' text='Home' type='btncn'/>
            </form>
            
            <br/>
            {(badResponseDetail !== '') ? 
              <p id='badResponseDetail' class="help is-danger">{badResponseDetail}</p>
            :
            <p id='goodResponseDetail' class="help is-success">{goodResponseDetail}</p>
            }
            <p id='fieldValid' class="help is-danger">{fieldValidation}</p>
          </div>
          </section>
          :
          <Redirect to='/'/> 
      }
    </div>
  )

}

/* This function allows you to update the password, requiring that the new password meets the established requirements. */

export function ChangePassword() {

	const context = useContext(userContext);
	const [newPassword, setNewPass] = useState('');
	const [oldPassword, setOldPass] = useState('');
	const [badResponseDetail, setBadResponseDetail] = useState('')
	const [goodResponseDetail, setGoodResponseDetail] = useState('')
	const [validationOldpass, setValidationOldpass] = useState('')
	const [validationNewpass, setValidationNewpass] = useState('')

	useEffect(() => {
    if(context.username === '') {
      const cookie = Cookies.getJSON("user");
      if(cookie !== undefined) {
          context.setUsername(cookie.username);
          context.setEmail(cookie.email);
          context.setToken(cookie.token);
          context.setIcon(cookie.icon);
      }
  }
  }, [context])
	
	const handleSubmit = (e) => {

		e.preventDefault()
  
    const headers = {
      Accept: "application/json",
      Authorization: "Bearer " + context.token,
      "Content-Type": "application/json"
    }

    const keys = {old_pwd: oldPassword, new_pwd: newPassword}

		if (validationNewpass === '' && validationOldpass === '' && newPassword !== '' && oldPassword !== '') {
			sendRequest("PUT", headers, keys, "http://127.0.0.1:8000/users/change_password")
				.then(async response => {
					const data = await response.json()

					if(!response.ok) {
            if(data.detail === 'Wrong old password') {
              return setValidationOldpass(data.detail)
            }
            return setBadResponseDetail(data.detail)
          } else {
						return setGoodResponseDetail(data.message)
					}
			})
		} else {
        if (newPassword === '') {
          setValidationNewpass('Please enter a new password.')
        }
        if (oldPassword === '') {
          setValidationOldpass('Please enter a old password.')
        }
    }
	}

	const handleNewpass = (e) => {
		setValidationNewpass('')
		setValidationOldpass('')
		setGoodResponseDetail('')
		setBadResponseDetail('')
		setNewPass(e.target.value)
	}

	const handleOldpass = (e) => {
		setValidationNewpass('')
		setValidationOldpass('')
		setGoodResponseDetail('')
		setBadResponseDetail('')
		setOldPass(e.target.value)
	}

	const validateFields = (e) => {
		var regExpPsw = /^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z]).{8,32}$/
    const field_name = e.target.name
    const field_value = e.target.value
    
    if (field_value.length === 0) {
			if (field_name === 'Old password') {
      	return setValidationOldpass(`${field_name} is required`)
			} else {
				return setValidationNewpass(`${field_name} is required`)
			}
    }
    
    if (field_value && (field_value.length < 8 || field_value.length > 54) && field_name === 'New password') {
			return setValidationNewpass('The password must contain between 8 and 54 characters')
		}

		if (field_name === 'New password' && !regExpPsw.test(field_value)) {
			return setValidationNewpass('The password must contant at least one number and one capital letter')
		}
    

    //If everything is correct I don't show a message.

		setValidationNewpass('')
		setValidationOldpass('')
    
  }
	
	return (
		<div>
			{ (Cookies.get("user") !== undefined) ?
          <section class='room-bg'>
          <div class='container has-text-centered mt-6'>
            <form onSubmit={handleSubmit}>
              <div class="field">
                  <label id='oldpass' class='room-label'>Old password</label>
                  <div>
                    <input type="password" name="Old password" id="Oldpass" 
                      value={oldPassword} onChange={handleOldpass} onBlur={validateFields}/> 
                  </div>
                  <div id="valOldpass" class="help is-danger">{validationOldpass}</div>
              </div>
							<div class="field">
                  <label id='newpass' class='room-label'>New password</label>
                  <div>
                    <input type="password" name="New password" id="Newpass" 
                      value={newPassword} onChange={handleNewpass} onBlur={validateFields}/> 
                  </div>
                  <div id="valNewpass" class="help is-danger">{validationNewpass}</div>
              </div>
              <input class='room-button mx-2 my-2' name='Change' type='submit' value='Change'/>
              <Button style='room-button mx-2 my-2' path='/home' text='Home' type='btncp'/>
            </form>
            
            <br/>
            {(badResponseDetail !== '') ? 
              <p id='badrsp' class="help is-danger">{badResponseDetail}</p>
            :
            <p id='goodrsp' class="help is-success">{goodResponseDetail}</p>
            }
            <p class="help is-danger">{}</p>
          </div>
          </section>
          :
          <Redirect to='/'/> 
      }
		</div>
	)
}