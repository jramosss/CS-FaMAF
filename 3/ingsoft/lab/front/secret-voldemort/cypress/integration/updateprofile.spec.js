context('Update nickname', () => {

  // https://on.cypress.io/interacting-with-elements
  describe('User is not logged in', () => {
    
    it('Try to access the updateprofile page\n [*] should render the login page', () => {
      cy.visit('http://localhost:3000/change_nickname')
      cy.get('h2')
      .contains('To play you need to be logged in')
    })

  })

  describe('User is logged in', () => {
    beforeEach(() => {
      // We login with a user to be able to test the component correctly
      cy.visit('/')

      cy.get('input[type=email]')
        .type('player1@example.com')
      
      cy.get('input[type=password]')
        .type('Heladera65')
  
      cy.get('input[type=submit]')
        .click()
    })

    /* It checks the correct rendering of the page */
    it('Click on change nickname\n [*] should render the update profile page', () => {
      cy.get('button[type=btncn]')
        .click()
      cy.get('label')
        .contains('New nickname')
      cy.get('input[name=Update]')
        .contains('Update')
        cy.get('button[type=btncn]')
        .contains('Home')
    })

    it('Try to update without filling the field.\n [*] should notify the field that is required', () => {
      cy.get('button[type=btncn]')
        .click()
      cy.get('input[type=submit]')
        .click()
      cy.get('p')
        .should('be.visible')
        .and("contain", 'Please enter a nickname to update it.')
    })

    it('Try to update the nickname with a too long one.\n [*] should notify the expected range', () => {
      cy.get('button[type=btncn]')
        .click()
      cy.get('input[type=text]')
        .type('NicknameTestTooLong')
      cy.get('input[type=submit]')
        .click()
      cy.get('div.help.is-danger')
        .should('be.visible')
        .and("contain", 'Nickname must contain between 3 and 15 characters')
    })

    it('Try to update the nickname with a too short one.\n [*] should notify the expected range', () => {
      cy.get('button[type=btncn]')
        .click()
      cy.get('input[type=text]')
        .type('12')
      cy.get('input[type=submit]')
        .click()
      cy.get('div.help.is-danger')
        .should('be.visible')
        .and("contain", 'Nickname must contain between 3 and 15 characters')
    })

    

    it('Try to update the nickname with an existing nickname.\n [*] should notify that the nickname is in use', () => {
      cy.get('button[type=btncn]')
      .click()
      cy.get('input[type=text]')
        .type('player1')
      cy.get('input[type=submit]')
        .click()
      cy.get('p.help.is-danger')
      .should('be.visible')
      .and("contain", 'Nickname already registered')
    })

    it('ALL OK.\n [*] the users nickname should be successfully changed, and rendered in the home page', () => {
      cy.get('button[type=btncn]')
      .click()
      cy.get('input[type=text]')
        .type('Testingnick')
      cy.get('input[type=submit]')
        .click()
      cy.get('p.help.is-success')
      .should('be.visible')
      .and("contain", 'You have changed your nickname succesfully')
    })

  })

})