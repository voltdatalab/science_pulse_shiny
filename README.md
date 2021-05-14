# Science Pulse

[**Science Pulse**](https://sciencepulse.org/eng/) is a social listening platform of the scientific community on Twitter. It shows a collection of trending tweets from a list of relevant profiles over the last 12h, and makes available an archive of posts and information on those profiles.

This is the code for the [**Science Pulse**](https://nucleojor.shinyapps.io/science_pulse_eng/) web application, English version. The **Science Pulse** is distributed under the MIT License.

## About 

The **Science Pulse** is a social listening app designed to bridge the gap between journalism and science in social media. Its goal is to help journalists find scientific content trending on social media and get to know new experts/specialists.

Scientists and journalists have a longstanding record of collaboration. But, as social media creates its own hard-to burst bubbles, sometimes a bridge is necessary to bring awareness about each other’s conversations. Journalists can benefit a lot from knowing what scientists and experts are sharing on social media.

The **Science Pulse** tries to be that bridge. In a time where scientific knowledge is in great demand, there are also great challenges around the way academic papers and preliminary conclusions are communicated. Data, conclusions and decisions can change fast.

It is hard to keep up with all social media has to offer nowadays, and even harder to follow hundreds of new people, even if you are interested in what they have to say. And even if you do follow them, some messages will be buried amongst hundreds of other posts and trending topics.

That is why we decided to make a tool to increase the discovery around the scientific conversation on social media, from a diverse set of experts, especially regarding the coronavirus pandemic. It is a way to increase knowledge but also leave some noise out of social media. We will give room for people that are brilliant in their fields, but might not have thousands of followers to engage.

## Who does it?

This project is led by ICFJ Knight Fellow [Sérgio Spagnuolo](https://twitter.com/sergiospagnuolo), in collaboration with [Volt Data Lab](https://voltdata.info/en/), a data-driven news agency he founded in São Paulo, Brazil. From Volt's team, [Lucas Gelape](https://twitter.com/lgelape) is responsible for the main structure of the app. Besides overseeing the project, Sérgio's main work on the code is focused on improving UI aspects. [Renata Hirota](https://twitter.com/renata_mh) also worked on the code. The images and logos were made by [Rodolfo Almeida](https://twitter.com/rodolfoalmd). The database infrastructure was developed by [Felippe Mercurio](https://twitter.com/ztock). [Jade Drummond](https://twitter.com/jade_dru) helps to curate profiles and gather information about them. Science Pulse is supported by the [International Center for Journalists (ICFJ)](https://www.icfj.org/) and by the Brazilian science journalism agency [Bori](https://abori.com.br/).

## How does it work?

The **Science Pulse** app is a [Shiny](https://shiny.rstudio.com/)-built app, hosted with [Shinyapps.io](https://www.shinyapps.io/). 

Every 20 minutes, we extract all tweets posted by a list of around 1,500 Twitter accounts of experts, academics, research institutions and universities who tweet mainly in English, Portuguese and Spanish. This list is constantly updated (to include or remove accounts) and profiles are chosen by their posted content pertinence and/or their academic affiliation. 

Tweets are extracted from the Twitter Free API, using the `rtweet` package. All tweets are stored in database, powered by [Postgres](https://www.postgresql.org/) and hosted on the Amazon Web Services cloud, from which we download the data to be used in the application. 

The app is structured as a modularized `navbarPage`, and includes six main tabs, which we describe below. Each tab has specifically designed algorithms to improve the discovery of diverse content (for more information on the algorithms, read our [methodology](https://sciencepulse.org/eng/methodology) section). In most tabs, users should choose if they want to see tweets in English, Portuguese or Spanish. 

Three tabs (**Trends**, **Explore** and **Covid-19 Special**) focus on revealing content from the last 12h, helping users to identify useful posts from the daily news cycle. The other three tabs incentivize users to dig deeper into our datasets. **Profiles** lets users find more information on the profiles we monitor and get recommendations of new profiles to follow. **Popularity** plots a graph so that user can compare engagement trends of up to 4 Twitter accounts. Finally, **Search Tweets** allow users to search for posts according to different filters and keywords.

### Trends

The app's main tab has three columns.The first two show trending tweets according to three different metrics: overperforming, popularity and discovery. They are designed to tap into different aspects of popularity and only consider posts written by the profiles we monitor: 

* *Overperforming* was based on Crowd Tangle's overperforming measure and focuses on identifying popular posts from an account, compared to the average number of interactions they usually get;
* *Popularity* ranks the most retweeted posts;
* *Discovery* ranks the tweets with the highest RT:followers ratio.

The last column (*Radar*) shows a random sample of five tweets from the last 12h. Users can sample new tweets by clicking on the button. This sample is also stratified in order to always show two tweets from non-male experts/academics.

### Explore

This tab shows further information on what's happening on our monitored posts over the last 12h. They reveal the most active accounts and the most tweeted hashtags in this period.

On three more columns, we show different trending tweets, who usually are not the most trending ones. The first two uses a k-means clustering algorithm to find popular but not the most trending tweets, and the last one is focused on what is being retweeted more by the profiles we monitor, thus it can include tweets from accounts not in our list.

### Popularity

On this tab, users can compare engagement trends of up to 4 Twitter accounts monitored by the **Science Pulse**, according to four different metrics: engagement trend, engagement rate, interactions' total and number of posted tweets.

### Profiles

Here, we show a table listing different characteristics from the accounts we follow. Some of them were available from the Twitter API -- such as their location, description and followers count -- but others were added by Science Pulse's team -- such as their type (institution or person), gender, field of study and institutional affiliation/google scholar profile.

Users can filter the tab according to different criteria and search for keywords. Moreover, in the Find New Experts table, we show a random sample of 5 from the least followed accounts in our list, focusing on publicizing more non-male and non-white experts/academics.

### Search Tweets

In this page, users can search for Tweets extracted by Science Pulse over the last 90 days according to several different criteria, such as showing/hiding non-verified profiles, retweets or replies. Also, it’s possible to filter the period of time they were posted. As in the Profiles tab, users can also search the table by keywords.

### Covid-19 Special

Finally, this tab filters tweets according to more than 30 keywords related to Covid-19 (they are listed in our [methodology](https://sciencepulse.org/eng/methodology)). The columns displayed here are also used on the Trends and Explore tabs.

