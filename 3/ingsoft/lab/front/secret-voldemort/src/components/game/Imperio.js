import React, { useContext, useState, useEffect } from 'react';
import Wand from '../../images/wand.png';
import { sendRequest } from '../../services/request';
import { userContext } from '../../user-context';
import Popup from 'reactjs-popup';
import 'reactjs-popup/dist/index.css';
import '../../custom.css';
import '../../popup_custom.css';

/* This component will get the cards to discard  for minister and director */

/* PROPS_NEEDED: room_name, players, my_name  */

export const Imperio = (props) => {

    const context = useContext(userContext);
    const [room_name, setRoomName] = useState('')
    const [modalText, setModalText] = useState('')
    const [player_list, setPlayerList]  = useState([])
    const [my_name, setMyName] = useState("")
    

    useEffect(() => {
       setRoomName(props.room_name)
       setPlayerList(props.players)
       setMyName(props.my_name)
    }, [props])

    /* A simple handle error function */
    const handleMessages = (message) => {
        let btnModal = document.getElementById("btnModalImperio")
        if(btnModal !== null){
            btnModal.click()
            setModalText(message)
        }
    }


    /* Function that cast the spell */
    const castImperio = (user) => {
        const headers = {
            Accept: "application/json",
            Authorization: "Bearer " + context.token,
            "Content-Type": "application/json"
        }
        try {
            const path = "http://127.0.0.1:8000/" + room_name + "/cast/imperius"
            const keys = {
                target_uname: user
            }
            sendRequest('PUT', headers, keys, path).then(async response => {
                const data = await response.json();
                if(!response.ok){ 
                    handleMessages(data.detail)
                }else{
                    // This is in case of something wrong with server, the buttons will dissapear
                    const list = document.getElementById("list_imperio_disable")
                    const buttons = document.getElementsByClassName("disable_btn_imperio")
                    list.style.display = "None"
                    Array.from(buttons).forEach(element => {
                        element.style.display = "None"
                    });
                }
            }).catch(error => {
                handleMessages("Ups! something went wrong.")
            })
        }catch(e){
            handleMessages("Error getting data from the current match")
        }
    }

    return (
        <div class="align-cntr">
            <Popup className='alert-modal' 
            trigger={<button id='btnModalImperio' 
            style={{display:"none"}}></button>} 
            modal position='right center'>
                    <p> 
                        {modalText}
                    </p>
            </Popup>
            <Popup className='spell-modal' 
                trigger={
                    <button id='btn-spell' class="btn-spell">
                        <figure class="image is-64x64">
                            <img height='128' width='128' src={ Wand }
                            alt=""/>
                        </figure>
                    </button>
                }
                    modal position='right center'
            > 
                <div class='container has-text-centered'>
                    <label>Select the next minister</label>
                    <ul id='list_imperio_disable' class='list_imperio_disable'>
                        {player_list.map((player, index) =>
                            (player !== my_name) ? (
                            <li class='i-playerlist li-spacing-modal'><span>{player}</span> 
                                <button 
                                class='panel-button disable_btn_imperio' 
                                onClick={() => castImperio(player)}>
                                Select</button>
                            </li>) : ""
                        )}
                    </ul>
                </div>
            </Popup>
        </div>
    );
}