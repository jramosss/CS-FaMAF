import React from 'react';
import { mount } from 'cypress-react-unit-test';
import { RoomTag } from './RoomTag';


context("Unit test for the listRoom components", () => {
    describe('Check the RoomTag', function () {
        it("Lookin for a room that is full", () => {
            mount(<RoomTag room={{name: 'FULLROOM', active_users: 5, max_players: 5}}/>);
            cy.get('div[id=FULLROOM').should('not.exist')
        })
    });
})
