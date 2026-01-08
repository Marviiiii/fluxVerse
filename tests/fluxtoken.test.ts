
import { describe, expect, it } from "vitest";
import { Cl, cvToValue } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const alice = accounts.get("wallet_1")!;
const bob = accounts.get("wallet_2")!;
const carol = accounts.get("wallet_3")!;

const readBalance = (account: string) =>
  simnet.callReadOnlyFn("fluxtoken", "get-balance", [Cl.principal(account)], account).result;

const readTotalSupply = () =>
  simnet.callReadOnlyFn("fluxtoken", "get-total-supply", [], deployer).result;

const unwrapUint = (value: any) => BigInt(value.value);

describe("fluxtoken", () => {
  it("mints for testing and transfers tokens", () => {
    const mintAmount = 1_000;
    const { result: mintResult } = simnet.callPublicFn(
      "fluxtoken",
      "mint-for-testing",
      [Cl.uint(mintAmount)],
      alice,
    );
    expect(mintResult).toBeOk(Cl.bool(true));

    const { result: supplyResult } = simnet.callReadOnlyFn("fluxtoken", "get-total-supply", [], alice);
    expect(supplyResult).toBeOk(Cl.uint(mintAmount));

    expect(readBalance(alice)).toBeUint(mintAmount);

    const { result: transferResult } = simnet.callPublicFn(
      "fluxtoken",
      "transfer",
      [Cl.uint(400), Cl.principal(alice), Cl.principal(bob), Cl.none()],
      alice,
    );
    expect(transferResult).toBeOk(Cl.bool(true));

    expect(readBalance(alice)).toBeUint(600);
    expect(readBalance(bob)).toBeUint(400);
  });

  it("handles approvals and transfer-from", () => {
    const { result: mintResult } = simnet.callPublicFn(
      "fluxtoken",
      "mint-for-testing",
      [Cl.uint(1_000)],
      alice,
    );
    expect(mintResult).toBeOk(Cl.bool(true));

    const { result: approveResult } = simnet.callPublicFn(
      "fluxtoken",
      "approve",
      [Cl.principal(bob), Cl.uint(300)],
      alice,
    );
    expect(approveResult).toBeOk(Cl.bool(true));

    const { result: allowanceResult } = simnet.callReadOnlyFn(
      "fluxtoken",
      "get-allowance",
      [Cl.principal(alice), Cl.principal(bob)],
      alice,
    );
    expect(allowanceResult).toBeUint(300);

    const { result: transferFromResult } = simnet.callPublicFn(
      "fluxtoken",
      "transfer-from",
      [Cl.uint(200), Cl.principal(alice), Cl.principal(carol), Cl.none()],
      bob,
    );
    expect(transferFromResult).toBeOk(Cl.bool(true));

    expect(readBalance(alice)).toBeUint(800);
    expect(readBalance(carol)).toBeUint(200);

    const { result: allowanceAfterResult } = simnet.callReadOnlyFn(
      "fluxtoken",
      "get-allowance",
      [Cl.principal(alice), Cl.principal(bob)],
      alice,
    );
    expect(allowanceAfterResult).toBeUint(100);
  });

  it("enforces minter authorization for mint and burn", () => {
    const { result: setMinterResult } = simnet.callPublicFn(
      "fluxtoken",
      "set-minter",
      [Cl.principal(alice)],
      deployer,
    );
    expect(setMinterResult).toBeOk(Cl.bool(true));

    const { result: mintResult } = simnet.callPublicFn(
      "fluxtoken",
      "mint",
      [Cl.uint(500), Cl.principal(bob)],
      alice,
    );
    expect(mintResult).toBeOk(Cl.bool(true));
    expect(readBalance(bob)).toBeUint(500);

    const { result: unauthorizedMint } = simnet.callPublicFn(
      "fluxtoken",
      "mint",
      [Cl.uint(1), Cl.principal(carol)],
      carol,
    );
    expect(unauthorizedMint).toBeErr(Cl.uint(401));

    const { result: burnResult } = simnet.callPublicFn(
      "fluxtoken",
      "burn",
      [Cl.uint(200), Cl.principal(bob)],
      alice,
    );
    expect(burnResult).toBeOk(Cl.bool(true));
    expect(readBalance(bob)).toBeUint(300);

    const supply = cvToValue(readTotalSupply());
    expect(unwrapUint(supply)).toBe(300n);
  });
});
