import React from 'react'
import '../../custom.css'
import Phoenix_order from '../../images/fo_won.jpg'
import {Route} from 'react-router-dom'
export const Fo_won  = () => {
    return(
        <div class='won_img' >
            <div class='triumph_text_fo'>The Phoenix Order has won, light reigns the world</div>
            <img src={ Phoenix_order } alt=""/>
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