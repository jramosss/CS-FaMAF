import React, { useContext, useState, useEffect } from 'react';
import  Death_Eater  from '../../images/DE_Dashboard.jpg';
import Phoenix from '../../images/PO_Dashboard.jpg';
import Expelliarmus from '../../images/expelliarmus.jpg'
import Cancel from '../../images/cancel.png';
import { sendRequest } from '../../services/request';
import { userContext } from '../../user-context';

/* This component will get the cards to discard  for minister and director */

/* PROPS_NEEDED: room_name, imgSrc, ind  */

export const Card = (props) => {

    const context = useContext(userContext);
    const [room_name, setRoomName] = useState('')
    const [imgSrc, setImgSrc] = useState('')
    const [index, setIndex] = useState(-1)

    useEffect(() => {
        var image = '';
        (props.image === 'Death Eater proclamation') ? (image = Death_Eater) : 
        ((props.image ==='Order of the Fenix proclamation') ? (image = Phoenix) :
         ((props.image === 'expelliarmus') ? (image = Expelliarmus) :
         image = Cancel))
       setImgSrc(image)
       console.log(image);
       setRoomName(props.room_name)
       setIndex(props.ind)
    }, [props])

    const discard = (index) => {
        const headers = {
            Accept: "application/json",
            Authorization: "Bearer " + context.token,
            "Content-Type": "application/json"
        }
        try {
            const path = "http://127.0.0.1:8000/" + room_name + "/discard"
            const keys = {
                card_index: index
            }
            sendRequest('PUT', headers, keys, path).then(async response => {
                const data = await response.json();
                if(!response.ok){ 
                    console.log(data.detail)
                }else{
                    console.log(data.message)
                }
            }).catch(error => {
                console.log("Ups! Something went wrong.")
            })
        }catch(e){
            console.log("Error getting data from the current match.")
        }
    }
    return (
        <figure class="image fig-inline-block">
            <img height='32' onClick={async () => {discard(index)}} 
            width='32' src={imgSrc} alt=""/>
        </figure>
    );
}