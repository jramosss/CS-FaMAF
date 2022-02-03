context('Create a new room', () => {

  // https://on.cypress.io/interacting-with-elements
  describe('User is not logged in', () => {
    it('Try access to change password page\n [*] should render login page', () => {
      cy.visit('http://localhost:3000/change_password')
      cy.get('h2[id=subtitle]')
        .should('be.visible')
        .and('contain', "To play you need to be logged in")
    })
  })

  describe('User is logged in', () => {

    beforeEach(() => {
      // We login with a user to be able to test the component correctly
      cy.visit('/')

      cy.get('input[type=email]')
        .type('player2@example.com')
      
      cy.get('input[type=password]')
        .type('Heladera65')
  
      cy.get('input[type=submit]')
        .click()
    })

    it('Try to change the password to one that is too long.\n [*] should notify that situation', () => {
      cy.get('button[id=change_password]')
        .click()
      cy.get('input[name="Old password"]')
        .type('Heladera65')
      cy.get('input[name="New password"]')
        .type('Thispassworditsverylongtestinggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg')
      cy.get('input[type=submit]')
        .click()
      cy.get('div[id=valNewpass]')
        .and('contain', "The password must contain between 8 and 54 characters")
    })

    it('Try to change the password to one that is too short.\n [*] should notify that situation', () => {
      cy.get('button[id=change_password]')
        .click()
      cy.get('input[id=Oldpass]')
        .type('Heladera65')
      cy.get('input[id=Newpass]')
        .type('T')
      cy.get('input[type=submit]')
        .click()
      cy.get('div[id=valNewpass]')
        .and('contain', "The password must contain between 8 and 54 characters")
    })
    
    it('Try to change the password without meet the requirement of capital letter.\n [*] should notify that situation', () => {
      cy.get('button[id=change_password]')
        .click()
      cy.get('input[id=Oldpass]')
        .type('Heladera65')
      cy.get('input[id=Newpass]')
        .type('sdkjsd32')
      cy.get('input[type=submit]')
        .click()
      cy.get('div[id=valNewpass]')
        .and('contain', "The password must contant at least one number and one capital letter")
    })

    it('Try to change the password by typing incorrectly the current one.\n [*] should notify that situation', () => {
      cy.get('button[id=change_password]')
        .click()
      cy.get('input[id=Oldpass]')
        .type('sdweqwew23')
      cy.get('input[id=Newpass]')
        .type('Sdkjsd32')
      cy.get('input[type=submit]')
        .click()
      cy.get('div[id=valOldpass]')
        .and('contain', "Wrong old password")
    })

    it('All okey.\n [*] should notify that the password was successfully changed', () => {
      cy.get('button[id=change_password]')
        .click()
      cy.get('input[id=Oldpass]')
        .type('Heladera65')
      cy.get('input[id=Newpass]')
        .type('Heladera66')
      cy.get('input[type=submit]')
        .click()
      cy.get('p[id=goodrsp]')
        .and('contain', "You have changed your password succesfully")
    })
  })

})