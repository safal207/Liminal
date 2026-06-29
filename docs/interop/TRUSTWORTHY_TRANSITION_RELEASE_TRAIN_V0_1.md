# Trustworthy Transition Release Train v0.1

**Status:** Draft release-candidate protocol  
**Tracking issue:** [Liminal #117](https://github.com/safal207/Liminal/issues/117)

## Purpose

This profile controls the stacked merge sequence for:

```text
PR #110  ecosystem compatibility v0.1
    ↓
PR #113  full lifecycle compatibility v0.2
    ↓
PR #116  durability assurance compatibility v0.3
```

It prevents a valid stack from silently becoming invalid because of:

- head SHA drift;
- base branch drift;
- broken predecessor ancestry;
- stale workflow or receipt pins;
- expired artifacts;
- a failed required status;
- squash or rebase merge of a stacked predecessor;
- retargeting a successor while changing its head.

## Frozen ancestry

The initial candidate is frozen as:

```text
main@426bf5c41a6215b0fef1e9ca59df00a880491c14
  → #110@2ac45920526526bbe42fcbcff7d7fca80cd667eb  (+6)
  → #113@b50ef4d90a76fbd5b5d2e835f85e348081f4110f  (+5)
  → #116@c2864dd9f57e962064f24b6cc6368f647ebaaea4  (+7)
```

The verifier checks each edge against GitHub's compare API. The child must be a
strict descendant, have `behind_by = 0`, preserve the exact merge base, and
contain the pinned delta commit count.

## Why merge commits are mandatory

A GitHub merge commit preserves both parents:

```text
parent 1 = exact target branch head
parent 2 = exact stacked PR head
```

This lets the next branch keep its existing head and ancestry.

Squash merge and rebase merge rewrite commit identity. They can make the next PR
show predecessor changes again, invalidate pinned head SHAs, and detach existing
receipts from the release candidate. Therefore the manifest requires:

```text
required_method = merge_commit
forbidden_methods = squash, rebase
```

For a merged stage, the live verifier fetches the merge commit and requires
exactly two parents equal to the pinned base SHA and PR head SHA.

## Candidate phases

The same manifest schema remains usable throughout the train:

```text
PRE_MERGE_FROZEN
STAGE_1_MERGED
STAGE_2_MERGED
STACK_MERGED_PENDING_FINAL
```

Merged stages must form a prefix. An open stage may never appear before a merged
stage.

The current `target.expected_head` must be:

- `target.initial_head` before any merge;
- the latest merged stage's merge commit afterward.

This intentionally fails closed if an unrelated commit lands on `main`. The
candidate must then be refreshed and revalidated instead of silently absorbing
new target state.

## Per-stage pins

Every stage pins:

- PR number;
- train state;
- head branch and head SHA;
- expected live base branch and SHA;
- immutable ancestry parent SHA;
- predecessor PR;
- delta commit count;
- exact successful workflow run IDs;
- required commit statuses;
- receipt workflow run, artifact ID, name, and SHA-256 digest.

`ancestry_parent_sha` never changes while the PR head remains unchanged.
`expected_base_sha` may change after retargeting the immediate successor to the
new `main` head.

## Live verification

Run offline schema and invariant checks:

```bash
python3 tools/verify_trustworthy_transition_release_train.py \
  --output artifacts/release-train-offline-receipt.json
```

Run live GitHub verification:

```bash
GITHUB_TOKEN=... \
python3 tools/verify_trustworthy_transition_release_train.py \
  --live \
  --output artifacts/release-train-live-receipt.json
```

The live mode checks:

1. current `main` head;
2. PR open/merged state;
3. exact head branch and SHA;
4. exact live base for open stages;
5. two-parent merge commit for merged stages;
6. predecessor ancestry and delta count;
7. pinned workflow run name, event, head, and successful conclusion;
8. receipt artifact name, digest, expiry state, run binding, and head binding;
9. latest required commit status, currently `CodeRabbit`.

The normalized live state is hashed into `live_state_digest`.

## Merge procedure

### Before merging #110

- complete mandatory Codex review;
- keep all three stage heads unchanged;
- run offline and live release-train verification;
- merge #110 using **Create a merge commit** only.

### After merging #110

1. record #110's merge commit SHA;
2. set stage 1 to `MERGED` and add `merge_commit_sha`;
3. update `target.expected_head` to that merge commit;
4. retarget #113 from `feat/ecosystem-pack-v0.1` to `main`;
5. do not rebase or modify #113 head;
6. update #113's expected base SHA to the new `main` head;
7. rerun all required workflows;
8. replace #113 workflow and receipt pins with the new successful runs;
9. set `candidate_state = STAGE_1_MERGED` and rerun live verification.

Repeat the same sequence for #113 and then #116.

## Blocker derivation

The verifier derives blockers from train state:

- `CODEX_REVIEW_PENDING` while Codex is not `PASSED`;
- `STACK_NOT_MERGED` until all three stages are merged;
- `FINAL_MAIN_MANIFEST_NOT_ISSUED` for every release candidate.

A release-candidate manifest always has:

```text
release_ready = false
```

After all stages merge, generate a separate final manifest from `main`. Do not
turn the candidate into a final release merely by changing a boolean.

## Invalidation

The candidate is invalid when any of these occurs:

```text
TARGET_HEAD_CHANGED
PINNED_PR_HEAD_CHANGED
PINNED_PR_BASE_CHANGED
ANCESTRY_CHANGED
WORKFLOW_RUN_CHANGED_OR_FAILED
RECEIPT_ARTIFACT_CHANGED_OR_EXPIRED
REQUIRED_COMMIT_STATUS_NOT_SUCCESSFUL
SUCCESSOR_HEAD_CHANGED_AFTER_RETARGET
NON_MERGE_COMMIT_STRATEGY_USED
```

Head drift invalidates that stage's receipt and every downstream pin that relies
on its ancestry.

## Security and trust boundary

The workflow uses read-only permissions for contents, pull requests, actions,
and commit statuses. It does not merge PRs or modify branches.

The release-train verifier does not replace:

- semantic conformance verification;
- CodeRabbit;
- mandatory Codex review;
- branch protection;
- repository-local CI;
- human release approval.

It proves that the release metadata still describes the live repository state
it claims to describe.
