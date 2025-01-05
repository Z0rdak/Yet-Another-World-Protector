/**
 * Credit: Peter Svensson for the initial implementation
 */
package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.HandlerUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.ExplosionDamageCalculator;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.FluidState;
import org.jetbrains.annotations.NotNull;

import java.util.Optional;

public class ExplosionDamageCalculatorInterceptor extends ExplosionDamageCalculator {
	protected ExplosionDamageCalculator nextBehavior;
	
	public ExplosionDamageCalculatorInterceptor(ExplosionDamageCalculator nextBehavior) 
	{
		this.nextBehavior = nextBehavior;
	}
	
	@Override
	public boolean shouldBlockExplode(Explosion explosion, BlockGetter blockGetter, BlockPos pos, BlockState state, float power) {
		FlagCheckEvent checkEvent = new FlagCheckEvent(pos, RegionFlag.EXPLOSION_BLOCK, explosion.level().dimension());
		FlagState flagState = HandlerUtil.processCheck(checkEvent);
		return flagState == FlagState.DENIED 
				? false 
				: nextBehavior.shouldBlockExplode(explosion, blockGetter, pos, state, power);
	}

	@Override
	public boolean shouldDamageEntity(Explosion explosion, Entity entity) {
		FlagCheckEvent checkEvent = new FlagCheckEvent(entity.blockPosition(), RegionFlag.EXPLOSION_ENTITY, explosion.level().dimension());
		FlagState flagState = HandlerUtil.processCheck(checkEvent);
		return flagState == FlagState.DENIED
				? false
				: nextBehavior.shouldDamageEntity(explosion, entity);
	}

	@Override
	public float getKnockbackMultiplier(Entity entity) {
		FlagCheckEvent checkEvent = new FlagCheckEvent(entity.blockPosition(), RegionFlag.EXPLOSION_ENTITY, entity.level().dimension());
		FlagState flagState = HandlerUtil.processCheck(checkEvent);
		return flagState == FlagState.DENIED
				? 0
				: nextBehavior.getKnockbackMultiplier(entity);
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
