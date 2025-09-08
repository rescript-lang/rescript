///// TODO: make use of these event handlers.
// // To get all the events
// transak.on(transak.ALL_EVENTS, (data) => {
// 		console.log(data)
// });
// // This will trigger when the user marks payment is made.
// transak.on(transak.EVENTS.TRANSAK_ORDER_SUCCESSFUL, (orderData) => {
//     console.log(orderData);
//     transak.close();
// });
type t

type walletAddressesData
/*
 EXAMPLE:
 {
 	networks : {
 		'erc20' : {address : '0xfF21f4F75ea2BbEf96bC999fEB5Efec98bB3f6F4'},
 		'bep2' : {address : 'bnb1dv5ps9vpj6clar79gkd0jrfmg8c0knrd6m090h', addressAdditionalData : '123456'}
 	},
 	coins : {
 		'BTC' : {address : 'bc1qlah8pucrmw8l3evszn8a7ay62gpyg00rzl7p2m'},
 		'DAI' : {address : '0xfF21f4F75ea2BbEf96bC999fEB5Efec98bB3f6F4'},
 		'BNB' : {address : 'bnb1dv5ps9vpj6clar79gkd0jrfmg8c0knrd6m090h', addressAdditionalData : '123456'}
 	}
 }
 */

type userData
/*
 EXAMPLE:
 {
   "firstName": "Satoshi",
   "lastName": "Nakamoto",
   "email": "email@gmail.com",
   "mobileNumber": "+19692154942",
   "dob": "1994-11-26",
   "address": {
     "addressLine1": "170 Pine St",
     "addressLine2": "San Francisco",
     "city": "San Francisco",
     "state": "CA",
     "postCode": "94111",
     "countryCode": "US",
   },
 };
 */

type initParams = {
  environment: string,
  apiKey: string,
  // Description: Your Transak API Key.
  // Example Value: 4fcd6904-706b-4aff-bd9d-77422813bbb7
  // Default Value:
  hostURL: string,
  // Description: Your domain name. The default value is window.location.origin. Our SDK requires this field to communicate with our system.
  // Example Value: https://yourCompany.com
  // Default Value:
  cryptoCurrencyCode: option<string>,
  // Description: "The code of the cryptocurrency you want the customer to purchase. If you pass a cryptoCurrencyCode, the currency will be selected by default and the customer won't be able to select another currency."
  // Example Value: CDAI
  // Default Value: ETH
  defaultCryptoCurrency: option<string>,
  // Description: "The default cryptocurrency you would prefer the customer to purchase. If you pass a defaultCryptoCurrency, the currency will be selected by default, but the customer will still be able to select another cryptocurrency. "
  // Example Value: CDAI
  // Default Value:
  cryptoCurrencyList: option<string>,
  // Description: A comma-separated list of cryptoCurrencies that you would allow your customers to buy. Only these cryptoCurrencies will be shown in the widget. This will be a string of comma separated values each of which will represent a valid cryptoCurrency code,"ETH,CDAI
  // Example Value: USDT"
  // Default Value:
  networks: option<string>,
  // Description: A comma-separated list of crypto networks that you would allow your customers to buy. Only these networks' cryptocurrencies will be shown in the widget. This will be a string of comma-separated values each of which will represent a valid network name. You can get the supporting networks by opening global.transak.com and then go to cryptocurrencies select screen. ,"ethereum,matic
  // Example Value: mainnet"
  // Default Value:
  walletAddress: option<string>,
  // Description: "The wallet public address that the purchased funds will be sent to. If you pass a valid wallet address, the customer won't be prompted to enter one. This parameter will be ignored if cryptoCurrencyCode is not passed. "
  // Example Value: 0x86349020e9394b2BE1b1262531B0C3335fc32F20
  // Default Value:
  walletAddressesData: option<walletAddressesData>,
  // Description: "Here you can pass multiple wallet addresses of the different cryptocurrencies & networks in the JSON object format. Use this query parameter if you are allowing multiple options to your users to buy a cryptocurrency. If you pass the valid wallet addresses, the customer won't be prompted to enter one. If walletAddress is passed, then this field will be ignored. "
  // Example Value:
  // Default Value:
  fiatCurrency: option<string>,
  // Description: The code of the fiat currency you want the customer to buy/sell cryptocurrency.
  // Example Value: INR
  // Default Value:
  countryCode: option<string>,
  // Description: The country's ISO 3166-1 alpha-2 code. The fiat currency will be display as per the country code. This parameter will be skipped if fiatCurrency is passed.
  // Example Value: IN
  // Default Value:
  fiatAmount: option<int>,
  // Description: An integer amount representing how much the customer wants to spend. Users can't change the fiat amount if this is passed. This parameter will be skipped if fiatCurrency is not passed.
  // Example Value: 1100
  // Default Value:
  defaultNetwork: option<string>,
  // Description: "The default network you would prefer the customer to purchase. If you pass a defaultNetwork, the network will be selected by default, but the customer will still be able to select another network. "
  // Example Value: matic
  // Default Value:
  defaultFiatAmount: option<int>,
  // Description: An integer amount representing how much the customer wants to spend. Users can change the fiat amount if this is passed. This parameter will be skipped if fiatCurrency is not passed.
  // Example Value: 1100
  // Default Value:
  paymentMethod: option<string>,
  // Description: "The payment method you want to show to the customer with purchasing. If you pass a paymentMethod, the payment method will be selected by default and the customer won't be able to select another payment method. This parameter will be skipped if fiatCurrency is not passed. Open this doc to get the possible values."
  // Example Value: upi Open this document to see more examples
  // Default Value:
  defaultPaymentMethod: option<string>,
  // Description: "The default payment method you would prefer the customer to purchase with. If you pass a defaultPaymentMethod, the payment method will be selected by default and the customer can also select another payment method.  This parameter will be skipped if fiatCurrency is not passed. Open this doc to get the possible values."
  // Example Value: upi Open this document to see more examples
  // Default Value:
  disablePaymentMethods: option<string>,
  // Description: A comma-separated list of payment methods you want to disable and hide from the customers. Open this doc to get the possible values.,"credit_debit_card
  // Example Value: upi Open this document to see more examples"
  // Default Value:
  email: option<string>,
  // Description: The email that will be used to identify your customer and their order (usually the email that they registered with your app).
  // Example Value: customer@mail.com
  // Default Value:
  userData: option<userData>,
  // Description: "Here you can pass your user's data like their name, address, date of birth in the object format. If you will pass all the basic user's data, the customer won't be prompted to enter it. "
  // Example Value:
  // Default Value:
  partnerOrderId: option<string>,
  // Description: An order ID that will be used to identify the transaction once a webhook is called back to your app. This can be your identifier to track your customers.
  // Example Value: 5e2f559511a9de73d364eeaf
  // Default Value:
  partnerCustomerId: option<string>,
  // Description: A customer ID that will be used to identify the customer that made the transaction once a webhook is called back to your app.
  // Example Value: 23487492
  // Default Value:
  accessToken: option<string>,
  // Description: "Pass the user's accessToken. Using this parameter, you can do widget + API integration easily by passing the user's KYC data via API and then show our widget for the payment."
  // Example Value: D8W3jTY1e7GNRMEOI3UJeQZ3j40lbl7NojyS8UkUrFb2BAnviy8kEPEB85WfkqFg
  // Default Value:
  redirectURL: option<string>,
  // Description: Transak will redirect back to this URL once the customer's has completed their purchase process. Please pass the valid URL otherwise it will not work.
  // Example Value: https://google.com
  // Default Value:
  disableWalletAddressForm: option<bool>,
  // Description: "When true, the customer will not be able to change the destination address of where the cryptocurrency is sent to."
  // Example Value: TRUE
  // Default Value: FALSE
  isAutoFillUserData: option<bool>,
  // Description: "When true, then the email address will be auto-filled, but the screen will not be skipped. User can edit their email address, basic data like first name & the address. This parameter will be ignored if email or userData are not passed."
  // Example Value: TRUE
  // Default Value: FALSE
  themeColor: option<string>,
  // Description: "The theme color code for the widget main color. It is used for buttons, links and highlighted text. Only hexadecimal codes are accepted."
  // Example Value: #000000
  // Default Value:
  height: option<string>,
  // Description: Height of the widget iFrame.
  // Example Value: 100.00%
  // Default Value: 100.00%
  width: option<string>,
  // Description: Width of the widget iFrame.
  // Example Value: 100.00%
  // Default Value: 100.00%
  hideMenu: option<bool>,
  // Description: "When true, then the customer will not see the menu options. This will hide the menu completely."
  // Example Value: TRUE
  // Default Value: FALSE
  hideExchangeScreen: option<bool>,
  // Description: "When true, then the customer will not see the home screen (exchange screen). This will hide the exchange screen completely, then the customer can't change the payment method, cryptocurrency, fiat amount. This parameter will be ignored if fiatAmount, fiatCurrency, cryptoCurrencyCode and paymentMethod are not passed. "
  // Example Value: TRUE
  // Default Value: FALSE
  isDisableCrypto: option<bool>,
  // Description: "When true, then the customer will not see anything about the cryptocurrency. This will hide all the information related to crypto completely. This parameter will be ignored if cryptoCurrencyCode, walletAddress is not passed. This parameter is helpful if you are creating an app for the users who do not know much about the cryptocurrencies."
  // Example Value: TRUE
  // Default Value: FALSE
  isFeeCalculationHidden: option<bool>,
  // Description: "When true, then the customer will not see our fee breakdown. The customer will only see the total fee. This parameter will be ignored if your fee (on top of us) is more than 1%. "
  // Example Value: TRUE
  // Default Value: FALSE
  exchangeScreenTitle: option<string>,
  // Description: To change the exchange screen title.
  // Example Value: Buy Crypto Instantly
  // Default Value: Buy crypto to your wallet
}

@new @module("@transak/transak-sdk")
external new_: initParams => t = "default"

@send external init: (t, unit) => unit = "init"
