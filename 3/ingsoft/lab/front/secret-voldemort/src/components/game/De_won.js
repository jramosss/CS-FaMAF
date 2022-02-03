import React from 'react'
import '../../custom.css'
import Death_eaters from '../../images/de_won.jpg'
import {Route} from 'react-router-dom'
export const De_won  = () => {
    return(
        <div class='won_img' >
            <div class='triumph_text_de'>Lord Voldemort has won, darkness reigns the world</div>
            <img src={ Death_eaters } alt=""/>
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