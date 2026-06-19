/**
 * Credit: Peter Svensson for the initial implementation
 */
package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.Flag;
import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.core.flag.FlagState;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.ExplosionDamageCalculator;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.FluidState;
import org.jetbrains.annotations.NotNull;

import java.util.Optional;

public class ExplosionDamageCalculatorInterceptor extends ExplosionDamageCalculator {
	protected ExplosionDamageCalculator nextBehavior;
	protected Level level;

	public ExplosionDamageCalculatorInterceptor(ExplosionDamageCalculator nextBehavior, Level level)
	{
		this.nextBehavior = nextBehavior;
		this.level = level;
	}

	@Override
	public boolean shouldBlockExplode(Explosion explosion, BlockGetter blockGetter, BlockPos pos, BlockState state, float power) {
		Flag flag = switch (explosion.getIndirectSourceEntity()) {
			case Creeper c -> FlagRegister.EXPLOSION_CREEPER_BLOCK;
			// case Player p -> RegionFlag.EXPLOSION_PLAYER_BLOCK;
			case null, default -> FlagRegister.EXPLOSION_BLOCK; // is null for dispenser etc
		};
		FlagCheckRequest checkEvent = new FlagCheckRequest(pos, flag, this.level.dimension());
		FlagState flagState = FlagEvaluator.processCheck(checkEvent);
		return flagState == FlagState.DENIED
				? false : nextBehavior.shouldBlockExplode(explosion, blockGetter, pos, state, power);
	}

	@Override
	public boolean shouldDamageEntity(Explosion explosion, Entity entity) {
		Flag flag = switch (explosion.getIndirectSourceEntity()) {
			case Creeper c -> FlagRegister.EXPLOSION_CREEPER_ENTITY;
			case null, default -> FlagRegister.EXPLOSION_ENTITY;
		};
		FlagCheckRequest checkEvent = new FlagCheckRequest(entity.blockPosition(), flag, explosion.level().dimension());
		FlagState flagState = FlagEvaluator.processCheck(checkEvent);
		return flagState == FlagState.DENIED
				? false : nextBehavior.shouldDamageEntity(explosion, entity);
	}

	// Since we don't have a reference here from the explosion to determine the source entity, 
	// we just go with the fact that if any of the two flags are denied we prevent knockback
	@Override
	public float getKnockbackMultiplier(Entity entity) {
		FlagCheckRequest checkExplosionEntityFlag = new FlagCheckRequest(entity.blockPosition(), FlagRegister.EXPLOSION_ENTITY, entity.level().dimension());
		FlagCheckRequest checkCreeperExplosionEntityFlag = new FlagCheckRequest(entity.blockPosition(), FlagRegister.EXPLOSION_CREEPER_ENTITY, entity.level().dimension());
		FlagState flagState1 = FlagEvaluator.processCheck(checkExplosionEntityFlag);
		FlagState flagState2 = FlagEvaluator.processCheck(checkCreeperExplosionEntityFlag);
		return flagState1 == FlagState.DENIED || flagState2 == FlagState.DENIED
				? 0	: nextBehavior.getKnockbackMultiplier(entity);
	}

	// Note: All other method implementations pass the call directly to the underlying ExplosionDamageCalculator

	@Override
	public @NotNull Optional<Float> getBlockExplosionResistance(Explosion explosion, BlockGetter blockGetter, BlockPos pos, BlockState blockState, FluidState fluidState) {
		return nextBehavior.getBlockExplosionResistance(explosion, blockGetter, pos, blockState, fluidState);
	}

	@Override
	public float getEntityDamageAmount(Explosion explosion, Entity entity, float amount) {
		return nextBehavior.getEntityDamageAmount(explosion, entity, amount);
	}
}
