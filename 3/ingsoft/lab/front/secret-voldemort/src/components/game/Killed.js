import React from 'react'
import '../../custom.css'
import de_won from '../../images/de_won.jpg'
import {Route} from 'react-router-dom'

export const Killed  = () => {
    return(
        <div class='won_img' >
            <div class='triumph_text_fo'>You were killed, you may rest in the lobby</div>
            <img src={ de_won } alt=""/>
            <Route render={({history}) => (
                <button
                    class='login-button is-medium is-fullwidht is-rounded mb-2 log-btn-margin triumph_button'
                    type='button'
                    onClick={() => { history.push('/home') }}
                >
                Go back
                </button>
            )} />
        </div>
    )
}