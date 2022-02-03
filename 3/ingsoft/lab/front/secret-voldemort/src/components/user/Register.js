import React, { useState, useEffect } from 'react';
import { Redirect } from "react-router-dom"
import Popup from 'reactjs-popup';
import 'reactjs-popup/dist/index.css';
import { sendRequest } from '../../services/request'
import '../../custom.css';
import '../../popup_custom.css';
const ALL_EMPTY = 1;
const NICK_OUT_OF_BOUNDS = 2;
const PASS_CONSTRAINTS = 3;
const NOT_EQUAL_PASS = 4;
const NOT_VALID_MAIL = 5;
const PASS_OUT_OF_BOUNDS = 6;
const ALREADY_REGISTERED = 409;

export const Register = (props) => {

    const [nameUser, setNameUser] = useState('')
    const [passUser, setPassUser] = useState('')
    const [passUser2, setPassUser2] = useState('')
    const [mailUser, setMailUser] = useState('')
    const [redir, setRedir] = useState(false)
    const [toPage, setToPage] = useState('')
    const [img, setImg] = useState(null)
    const [showModal, setShowModal] = useState(false)
    // methods
    const handleChange = (e) => {
        this.setState({[e.target.name]: e.target.value})
    }
    
    useEffect(() => {
        let btnModal =  document.getElementById("btnModal")
        if(showModal && null !=  btnModal){
            btnModal.click()
        }
    })
    
    /* This function is for server errors and validate inputs */
    const handleErrors = (status, detail) => {
        let nick = document.getElementById("nickValid")
        let psw = document.getElementById("pswValid")
        let psw2 = document.getElementById("psw2Valid")
        let email = document.getElementById("emailValid")
        let all = document.getElementById("allEmptyValid")
        // set empty
        var msgElements = document.getElementsByClassName("validation")
        Array.from(msgElements).forEach(element => {
            element.innerText = ""
        });
        let formatedDetail = '* ' + detail
        // contemplating cases
        switch(status){
            case ALL_EMPTY:
                all.innerText = formatedDetail
                break
            case NICK_OUT_OF_BOUNDS:
                nick.innerText = formatedDetail
                break
            case PASS_CONSTRAINTS:
                psw.innerText = formatedDetail
                break
            case NOT_EQUAL_PASS:
                psw.innerText = formatedDetail
                psw2.innerText = formatedDetail
                break
            case NOT_VALID_MAIL:
                email.innerText = formatedDetail
                break
            case PASS_OUT_OF_BOUNDS:
                psw.innerText = formatedDetail
                break
            case ALREADY_REGISTERED:
                all.innerText = formatedDetail
                break
            default:
                Array.from(msgElements).forEach(element => {
                    element.innerText = ""
                });
                break
        }
        Array.from(msgElements).forEach(element => {
            element.style.display = 'block'
        });
    }
     /* Deprecated */
    const handleChangeImg = (e) => {
        setImg(e.target.files[0])
    }

    /* Form validation control */
    const validateAndSubmit = (e) => {
        var msgElements = document.getElementsByClassName("validation")
        Array.from(msgElements).forEach(element => {
            element.style.display = 'none'
        });
        e.preventDefault()
        var regExpMail = /^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$/
        var regExpPsw = /^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z]).{8,32}$/
        if(nameUser === ""){
            handleErrors(ALL_EMPTY,"There are empty fields")
        }else if(nameUser.length < 3 || nameUser.length > 15){ 
            handleErrors(NICK_OUT_OF_BOUNDS, "The nickname must contain between 8 and 15 characters")
        }else if(passUser === ""){
            handleErrors(ALL_EMPTY,"There are empty fields")
        }else if(passUser.length < 8 || passUser.length > 54){
            handleErrors(PASS_OUT_OF_BOUNDS, "The password must contain between 8 and 54 characters")
        }else if(!regExpPsw.test(passUser)){
            handleErrors(PASS_CONSTRAINTS, "The password must contain at least one number and one capital letter")
        }else if(passUser !== passUser2){
            handleErrors(NOT_EQUAL_PASS, "The passwords are not equal")
        }else if(mailUser === ""){
            handleErrors(ALL_EMPTY,"There are empty fields")
        }else if( !regExpMail.test(mailUser)){
            handleErrors(NOT_VALID_MAIL, "You must select a valid e-mail")
        }else {
            sendData()
        }
    }

    /* Submit function  */
    const sendData = () => {
        const path = 'http://127.0.0.1:8000/users/register'
        const head = {
            Accept: "application/json",
            "Content-Type": "application/json"
          }
        var opts = `{
            "username": "${nameUser}",
            "password": "${passUser}",
            "email": "${mailUser}" ,
            "icon" : "${img}"
        }`
        sendRequest('POST',head,opts, path)
        .then(async response => {
            const data  = await response.json()
            if(response.ok){
                setToPage('/home')
                setShowModal(true)
                setTimeout(() => {setRedir(true)}, 1200);
            }else {
                handleErrors(ALREADY_REGISTERED, data.detail)
            }
        })
    }
    const handleRedirect = () => {
        setToPage('/home')
        setRedir(true)
    }
    if(redir){
        return <Redirect to={toPage} />    
    }
    return(
        <section>
            <div className="container reg-bg py-6 px-6">
                <Popup className='alert-modal' trigger={<button id='btnModal' style={{display:"none"}}></button>} modal position='right center'>
                    <p id='p_register'>
                        User created, a verification email was sent to {mailUser}
                    </p>
                </Popup>
                <h1 class='reg-title has-text-centered'>User registration</h1>
                <form encType="multipart/form-data" onSubmit={e => validateAndSubmit(e)}>
                    <div class='field'>
                        <div class='container'>
                            <div class='column is-3'>
                            <label class='login-label is-large'>Nickname: </label>
                                <input class='login-input' type="text" name="nameUser" id="nameUser" 
                                    value={nameUser} onChange={e => setNameUser(e.target.value)}/>
                                <div id='nickValid' class='validation'></div>
                            </div>
                        </div>
                    </div>
                    <div class='field'>
                        <div class='container'>
                            <div class='column is-3'>
                                <label class='login-label is-large'>Password: </label>
                                <input class='login-input' type="password" name="passUser" id="passUser" 
                                    value={passUser} onChange={e => setPassUser(e.target.value)}/> <br/>
                                <div id='pswValid' class='validation'></div>
                            </div>
                        </div>
                    </div>
                    <div class='field'>
                        <div class='container'>
                            <div class='column is-3'>
                                <label class='login-label is-large'>Repeat password: </label>
                                <input class='login-input' type="password" name="passUser2"
                                    id="passUser2" 
                                    value={passUser2} 
                                    onChange={e => setPassUser2(e.target.value)}/>
                                <div id='psw2Valid' class='validation'></div>
                            </div>
                        </div>
                    </div>
                    <div class='field'>
                        <div class='container'>
                            <label class='login-label is-large'>E-mail: </label>
                            <div class='column is-3'>
                                <input class='login-input' type="text" name="mailUser" 
                                    id="mailUser" 
                                    value={mailUser} 
                                    onChange={e => setMailUser(e.target.value)}/> <br/>
                                <div id='emailValid' class='validation'></div>
                                <div id='allEmptyValid' class='validation'></div>
                            </div>
                        </div>
                    </div>
                    <div class='container'>
                        <input class='login-button is-large mx-2 is-rounded' type="submit" 
                            id="regBtn" name="regBtn" value="Create account"/>
                        <input class='login-button is-large mx-2 is-rounded' type="button" 
                            id="cancelBtn" name="cancelBtn" 
                            onClick={handleRedirect} value="Cancel"/>
                    </div>
                </form>
            </div>
        </section>
    )
}