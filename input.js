function MusicPlayer({ user, songs }) {
  const title = songs[0].desc;
  const items = Array.from(
    songs
  ).unshift(2, 0, makeSong(user));
  
  // Just playing around with my new compiler
  items.push(MY_FAV_SONG)
  // ...
}