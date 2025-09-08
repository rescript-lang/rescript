module PrivacyPolicy = {
  let text = `Nook Exchange ("us", "we", or "our") operates nook.exchange (the "Site"). This page informs you of our policies regarding the collection, use and disclosure of Personal Information we receive from users of the Site.

We use your Personal Information only for providing and improving the Site. By using the Site, you agree to the collection and use of information in accordance with this policy.

1. INFORMATION COLLECTION AND USE

While using our Site, we may ask you to provide us with certain personally identifiable information that can be used to contact or identify you. Personally identifiable information may include, but is not limited to your email address ("Personal Information"). We collect your email address should you choose to create an account. While we ask for your birth date at registration to confirm that you are 13 years or older, we do not store this information.

2. LOG DATA

Like many site operators, we collect information that your browser sends whenever you visit our Site ("Log Data").

This Log Data may include information such as your computer's Internet Protocol ("IP") address, browser type, browser version, the pages of our Site that you visit, the time and date of your visit, the time spent on those pages and other statistics.

In addition, we may use third party services such as Google Analytics that collect, monitor and analyze this data.

3. COMMUNICATIONS

We may use your Personal Information to contact you if necessary for assistance with your account, or if we need to send a site-wide notice to all of our users. If we add a newsletter for regular updates in the future, it will be an opt-in service and you will not be subscribed by default.

4. COOKIES

Cookies are files with small amount of data, which may include an anonymous unique identifier. Cookies are sent to your browser from a web site and stored on your computer's hard drive.

Like many sites, we use "cookies" to collect information. You can instruct your browser to refuse all cookies or to indicate when a cookie is being sent. However, if you do not accept cookies, you may not be able to use some portions of our Site. We send a session cookie to users who log in and refusal to accept this cookie will prevent your account from working.

5. SECURITY

The security of your Personal Information is important to us, but remember that no method of transmission over the Internet, or method of electronic storage, is 100% secure. While we strive to use commercially acceptable means to protect your Personal Information, we cannot guarantee its absolute security.

6. CHANGES TO THIS PRIVACY POLICY

This Privacy Policy will remain in effect except with respect to any changes in its provisions in the future, which will be in effect immediately after being posted on this page.

We reserve the right to update or change our Privacy Policy at any time and you should check this Privacy Policy periodically. Your continued use of the Site after we post any modifications to the Privacy Policy on this page will constitute your acknowledgment of the modifications and your consent to abide and be bound by the modified Privacy Policy.

7. CONTACT US

If you have any questions about this Privacy Policy, please contact us at hi"AT"nook.exchange.`

  module Styles = {
    open Css
    let root = style(list{
      padding2(~v=zero, ~h=px(16)),
      margin3(~top=px(48), ~bottom=px(32), ~h=auto),
      maxWidth(px(720)),
    })
    let body = style(list{whiteSpace(#preLine)})
  }

  @react.component
  let make = () =>
    <div className=Styles.root>
      <h2> {React.string("Privacy Policy")} </h2>
      <div className=Styles.body> {React.string(text)} </div>
    </div>
}

module TermsOfService = {
  let text = `
3. ANTI-SPAM POLICY.

Use of our website in any way deemed to be spam by the administration will result in termination of your account. This includes creating a username or list that encourages users to visit another website or use another service. Attempting to contact other users of this site in any way for purposes deemed to be spam will not be tolerated.

4. MODIFICATIONS AND TERMINATIONS.

These terms and conditions may change from time to time. If such changes are made, they will be effective immediately. If you disagree with changes that have been made to these terms, you should not use this website.

We may terminate these terms and conditions of use for any reason and at any time without notice to you.

If you are concerned about these terms and conditions of use, you should read them each time before you use our website.

5. LICENSEE STATUS.

You understand and agree that your use of our website is limited and non-exclusive as a revocable licensee. We may terminate your license to use our website, and access to our website, for any reason, and without giving you notice.

6. CONTENT OWNERSHIP.

All in-game content on this website is the intellectual property of Nintendo. As a result, we nor you may claim ownership over any lists you create on this website.

7. DISCLAIMERS AND LIMITATIONS OF LIABILITY.

The information on our website is provided on an "as is," "as available" basis. You agree that your use of our website is at your sole risk. We disclaim all warranties of any kind, including but not limited to, any express warranties, statutory warranties, and any implied warranties of merchantability, fitness for a particular purpose, and non-infringement. We do not warrant that our website will always be available, access will be uninterrupted, be error-free, meet your requirements, or that any defects in our website will be corrected.

Information on our website should not necessarily be relied upon and should not to be construed to be professional advice from us. We do not guarantee the accuracy or completeness of any of the information provided, and are not responsible for any loss resulting from your reliance on such information.

If your jurisdiction does not allow limitations on warranties, this limitation may not apply to you. Your sole and exclusive remedy relating to your use of the site shall be to discontinue using the site.

Under no circumstances will we be liable or responsible for any direct, indirect, incidental, consequential (including damages from loss of business, lost profits, litigation, or the like), special, exemplary, punitive, or other damages, under any legal theory, arising out of or in any way relating to our website, your website use, or the content, even if advised of the possibility of such damages.

8. OBSCENE AND OFFENSIVE CONTENT.

We are not responsible for any obscene or offensive content that you receive or view from others while using our website. However, if you do receive or view such content, please contact us by e-mail at hi"AT"nook.exchange. Although we are not obligated to do so, we reserve the right to monitor, investigate, and remove obscene or offensive material posted to our website.

9. INDEMNIFICATION.

You understand and agree that you will indemnify, defend and hold us and our affiliates harmless from any liability, loss, claim and expense, including reasonable attorney's fees, arising from your use of our website or your violation of these terms and conditions.

10. COMPLIANCE WITH GOVERNING LAW AND DISPUTE RESOLUTION.

You agree to obey all applicable laws while using our website.

11. SEVERABILITY OF THESE TERMS AND CONDITIONS.

If any part of these terms and conditions of use are determined by a court of competent jurisdiction to be invalid or unenforceable, that part shall be limited or eliminated to the minimum extent necessary so that the remainder of these terms and conditions are fully enforceable and legally binding.

12. HOW TO CONTACT US.

Any questions or concerns about these terms and conditions of use should be brought to our attention by e-mail to hi"AT"nook.exchange. and providing us with information relating to your concern.

13. ENTIRE AGREEMENT.

These terms and conditions, including the policies incorporated herein by express reference, constitutes your entire agreement with us with respect to your use of our website.`

  module Styles = {
    open Css
    let root = style(list{
      padding2(~v=zero, ~h=px(16)),
      margin3(~top=px(48), ~bottom=px(32), ~h=auto),
      maxWidth(px(720)),
    })
    let body = style(list{whiteSpace(#preLine)})
  }

  @react.component
  let make = () =>
    <div className=Styles.root>
      <h2> {React.string("Terms of Service")} </h2>
      <div className=Styles.body>
        {React.string(`1. INTRODUCTION AND ACCEPTING THE TERMS

Welcome to Nook Exchange! These Terms of Service ("Terms"), which include and hereby incorporate the Privacy Policy, Nook Exchange ("us", "our", or "we") and you ("you" or "your"). By using or accessing the website located at https://nook.exchange, you agree (i) that you are 13 years of age or older, (ii) if you are the age of majority in your jurisdiction or over, that you have read, understood, and accept to be bound by the Terms, and (iii) if you are between 13 and the age of majority in your jurisdiction, that your legal guardian has reviewed and agrees to these Terms.

2. PRIVACY POLICY IS PART OF THESE TERMS AND CONDITIONS.

`)}
      </div>
      <div>
        {React.string(
          "Our privacy policy is part of, and subject to, these terms and conditions of use. You may view our privacy policy on this website ",
        )}
        <a href="/privacy-policy"> {React.string("here")} </a>
        {React.string(".")}
      </div>
      <div className=Styles.body> {React.string(text)} </div>
    </div>
}
