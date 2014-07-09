Haskell BitTorrent
===

A BitTorrent client written in Haskell (more accurately a leeching client). 

### Motivation
- explore how networking and concurrency works in a purely functional language
- learn Haskell
- learn how BitTorrent works

### Flow of the Program
- a `.torrent` is passed as a command-line argument
- that file is read in and decoded into a `MetaInfo` value, which is just a `BenDict` using Bencode.hs
- From that `MetaInfo`, enough information is extracted to create the tracker GET request. That GET is sent to the tracker and the tracker's response is decoded and a list of peers is extracted. (Tracker.hs:announceTracker) 
- For each peer, two threads are launched to listen to the peer and talk to the peer concurrently (PeerManage.hs:startPeers , PeerManage.hs:startPeer, Peer.hs:listenToPeer, Peer.hs:talkToPeer)
- a `Torrent` is passed to each peer upon starting and the `Torrent` contains the information for the torrent and the buffer of the actual content. 
- `listenToPeer` decodes messages and does the appropriate action (changing the peer's state or consuming a Piece)
- When a piece is consumed (Torrent.hs:consumeBlock), the `Torrent` adds it to it's `pieceBuffer` and when it has an entire piece, it writes it out to disk.
- `talkToPeer` sends messages to the peer. If the peer is choking us, then we send an `Interested` message every few seconds until they unchoke us. When we are unchoked, we send `Request` messages until the torrent is complete

### TODO
- Seed torrents
- Handle exceptions/errors
  - if any exception is thrown, the thread quits for that peer
  - Many things are optimistic and have zero error handling (Bencode decoding, message deserialization)
- once a request for a block is made, we assume we will get that block. There is no tracking of which blocks we actually receive so we never re-request
- queuing of requests
- logging mechanism
- send keepAlive messages                               