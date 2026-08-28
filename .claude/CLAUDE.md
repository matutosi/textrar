# textrar プロジェクト

## check の生成物の後始末

- **`R CMD check` などで作られる `*.tar.gz` は，役割が終わったら削除する**．
  結果を確認し終えたら (CRAN へ出す場合は提出が済んだら) 消してよい．
  DESCRIPTION とソースから何度でも作り直せるため，残しておく理由がない．
- 同じ理由で，`*.Rcheck/` (check の作業ディレクトリ) も確認が済んだら消す．
- 補足: `*.tar.gz` を作るのは `R CMD build` / `devtools::build()` で，
  `devtools::check()` は既定で一時ディレクトリに作るためプロジェクト直下には残らない．
  プロジェクト直下に残るのは `R CMD build` を直接実行したときが多い．
  どちらの経路でできたものでも，見つけたら消す．

## 鍵の扱い

- **鍵はコードに書かない．`~/.Renviron` (Windows の R では `C:\Users\<user>\Documents\.Renviron`)
  に置き，`Sys.getenv()` で読む**．変数名は `TEXTRA_API_KEY`・`TEXTRA_API_SECRET`・`TEXTRA_NAME`．
  `gen_params()`・`get_token()` はこれらを既定値にしてある．
- **プロジェクト直下に `.Renviron` を置かない**．`D:\Dropbox\todo\` は3台へ同期されるため，
  `.gitignore` に足しても Dropbox には乗る．ホーム直下は同期されないのでそちらが正しい．
- **同期されないので，鍵は3台それぞれで設定する**．運搬に Dropbox を使わない．
- `tools/` には旧鍵をベタ書きした原型スクリプトが残っている (`.gitignore` 済み)．
  **ここへ新しい鍵を書き戻さない**．

## 進捗状況

### 現在の状態

- 2026-08-28 17:21 (MATUTOSI_DP)
  **R2・R3・R5 を実施し，版を 0.9.0 にした**．応答の取り出しを普通の `$` に戻して
  `Depends` を R (>= 3.6) へ下げ，API の失敗を `stop()` にし，低水準の3関数を非推奨にした．
  テスト 50 件通過 (実 API 込み)．`R CMD check` は 0/0/0．

- 2026-08-28 16:52 (MATUTOSI_DP)
  **testthat (edition 3) を導入した**．通信しないテスト 30 件と，実 API の 4 件．
  実 API の分は `TEXTRA_TEST_LIVE` が無ければ skip するので，毎回の check で枠を消費しない．
  `devtools::test()` は 34 件通過 (実 API 込み)．

- 2026-08-28 16:36 (MATUTOSI_DP)
  **鍵の漏洩を塞ぎ，環境変数から読む形へ移した** (`develop` に3コミット)．
  `R CMD check` は 0 errors / 0 warnings / 0 notes．実 API で往復も確認済み．
  新しい鍵はユーザが再発行して `.Renviron` へ設定済み．

### 経緯: CRAN で鍵が公開されていた件 (2026-08-28 に発覚・対処)

- `.Rbuildignore` の `^\tools$` は，正規表現として `\t` がタブの escape になるため
  **何にもマッチせず**，鍵をベタ書きした `tools/` がソースパッケージに同梱されていた．
- **CRAN の `textrar_0.8.0.tar.gz` と GitHub の CRAN ミラー `cran/textrar` で実際に読める状態**
  だった (2024-04-23 の公開以来)．CRAN バイナリ・自分の GitHub リポジトリ・git 履歴は無事．
- **旧鍵は失効・再発行済み**．ミラーに残るため，取り消しではなく再発行が対処．
- 併せて `ssl_verifypeer = FALSE` (証明書検証の無効化) も削除した．

### 次にやること

- **【完了 2026-08-28】testthat の導入**．実 API のテストを手元で回すときは
  `TEXTRA_TEST_LIVE` を立てる (`$env:TEXTRA_TEST_LIVE = '1'`)．無いと skip される．
- **【完了 2026-08-28】R3: API の失敗を `stop()` にした**．
  **エラーでも HTTP は 200 で返り，`resultset$code` に番号が入る** (実測)．
  `stop_for_status()` だけでは足りないので，`check_api_code()` で `error`・
  `resultset$code`・HTTP の3つを見ている．コード 510 は `message` が空なので，
  エラー文には必ず番号を出すこと．
- **【完了 2026-08-28】R2: `` `$`(_, "x") `` を普通の `$` に戻した**．
  `Depends` は R (>= 3.6) (httr の下限)．
- **【完了 2026-08-28】R5: b の形 (非推奨の経過を置く)**．
  `post_request()`・`extract_result()`・`base_url()` は警告を出す包みになり，
  実装は `api_post()`・`api_extract()`・`api_base_url()` へ移した．
  **次の版 (0.10.0 か 1.0.0) で `@export` ごと削除する**．CRAN 上の逆依存はゼロ．
- **【判断待ち】CRAN へ 0.9.0 を出すか**．出すなら `cran-comments.md` が
  0.8.0 の初回提出のまま (再提出の記述が残っている) なので書き直しが要る．
- `develop` が `main` より先行している (未 merge)．CRAN 版の `main` をいつ揃えるか．
- **push はまだしていない**．
