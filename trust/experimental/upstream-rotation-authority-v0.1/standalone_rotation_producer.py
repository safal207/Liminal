#!/usr/bin/env python3
import base64, hashlib, json, sys
from copy import deepcopy
from pathlib import Path
from cryptography.hazmat.primitives.serialization import load_pem_public_key
from cryptography.exceptions import InvalidSignature

def canon(o):
    return (json.dumps(o, sort_keys=True, separators=(",", ":")) + "\n").encode("utf-8")

def h(o):
    return hashlib.sha256(canon(o)).hexdigest()

def verify(pub, payload, sig_b64):
    try:
        pub.verify(base64.b64decode(sig_b64), canon(payload))
    except InvalidSignature as e:
        raise SystemExit("control_plane_signature_invalid") from e

inp = json.loads(Path(sys.argv[1]).read_text())
pub = load_pem_public_key(inp["control_plane_public_key_pem"].encode())
verify(pub, inp["producer_contract"], inp["producer_contract_signature_b64"])
verify(pub, inp["authorization_contract"], inp["authorization_contract_signature_b64"])
verify(pub, inp["rotation_intent"], inp["rotation_intent_signature_b64"])

pc = inp["producer_contract"]
ac = inp["authorization_contract"]
intent = inp["rotation_intent"]
reg0 = inp["baseline_registry"]
man0 = inp["baseline_manifest"]

if h(pc) != intent["rotation_contract_sha256"]:
    raise SystemExit("rotation_contract_digest_mismatch")
if h(ac) != intent["authorization_contract_sha256"]:
    raise SystemExit("authorization_contract_digest_mismatch")
if h(reg0) != intent["previous_registry_sha256"]:
    raise SystemExit("previous_registry_digest_mismatch")
if h(man0) != intent["previous_manifest_sha256"]:
    raise SystemExit("previous_manifest_digest_mismatch")
if reg0["active_generation"] != intent["from_generation"]:
    raise SystemExit("from_generation_mismatch")
if intent["to_generation"] != intent["from_generation"] + pc["transition_contract"]["generation_delta"]:
    raise SystemExit("generation_delta_mismatch")
if ac["logical_rotation_id"] != pc["logical_rotation_id"] or intent["logical_rotation_id"] != pc["logical_rotation_id"]:
    raise SystemExit("logical_rotation_id_mismatch")
if ac["decision"] != pc["output_reason"] or intent["required_reason"] != pc["output_reason"]:
    raise SystemExit("rotation_reason_mismatch")

target = intent["target_root"]
old = man0["roots"][target["name"]]
if target["workflow_path"] != old["workflow_path"]:
    raise SystemExit("target_workflow_path_mismatch")
if target["git_blob_sha"] != old["git_blob_sha"]:
    raise SystemExit("target_workflow_blob_changed")
if target["workflow_sha"] == old["workflow_sha"]:
    raise SystemExit("rotation_root_unchanged")

man1 = deepcopy(man0)
man1["generation"] = intent["to_generation"]
man1["previous_manifest_sha256"] = h(man0)
man1["roots"][target["name"]]["workflow_sha"] = target["workflow_sha"]
man1["roots"][target["name"]]["git_blob_sha"] = target["git_blob_sha"]

manifest_path = "drill/generation-1-manifest.json"
manifest_digest = h(man1)
entry = {"generation": intent["to_generation"], "manifest_path": manifest_path, "manifest_sha256": manifest_digest}
reg1 = {
    "schema_version": reg0["schema_version"],
    "active_generation": intent["to_generation"],
    "active_manifest_sha256": manifest_digest,
    "history": [*reg0["history"], entry],
}

if reg1["history"][:-1] != reg0["history"]:
    raise SystemExit("registry_history_not_append_only")
if man1["previous_manifest_sha256"] != reg0["active_manifest_sha256"]:
    raise SystemExit("previous_manifest_digest_mismatch")
for root_name in ("builder", "verifier"):
    old_sha = man0["roots"][root_name]["workflow_sha"]
    new_sha = man1["roots"][root_name]["workflow_sha"]
    if new_sha != old_sha and reg0["active_generation"] > 0:
        raise SystemExit("unexpected_historical_root_context")
for material_name in ("builder_environment_policy", "verifier_dependency_lock"):
    if man1["policy_material"][material_name]["sha256"] != man0["policy_material"][material_name]["sha256"]:
        raise SystemExit("policy_material_changed")

out = {
    "schema": "liminal-external-rotation-producer-result/v0.1",
    "verified": True,
    "reason": pc["output_reason"],
    "logical_rotation_id": pc["logical_rotation_id"],
    "rotation_contract_sha256": h(pc),
    "authorization_contract_sha256": h(ac),
    "control_plane_authorization_intent_sha256": h(intent),
    "previous_registry_sha256": h(reg0),
    "current_registry_sha256": h(reg1),
    "previous_manifest_sha256": h(man0),
    "current_manifest_sha256": h(man1),
    "from_generation": intent["from_generation"],
    "to_generation": intent["to_generation"],
    "generation_1_manifest": man1,
    "generation_1_registry": reg1,
}
Path(sys.argv[2]).write_bytes(canon(out))
print(json.dumps(out, sort_keys=True, separators=(",", ":")))
