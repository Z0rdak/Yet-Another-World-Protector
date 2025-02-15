package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.FlagEvaluator;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FarmBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.HandlerUtil.*;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(FarmBlock.class)
public abstract class FarmLandBlockMixin extends Block {

    // Dummy constructor needed to allow inheriting from Block, which in turn is needed to call super.fallOn()
    // This constructor is not actually injected.
    public FarmLandBlockMixin(Properties properties) {
        super(properties);
    }

    @Inject(method = "fallOn", at = @At(value = "HEAD"), cancellable = true)
    private void onTrampleFarmland(Level world, BlockState state, BlockPos pos, Entity trampler, float fallDistance, CallbackInfo ci) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, TRAMPLE_FARMLAND, getDimKey(world), null);
            if (Services.EVENT.post(checkEvent)) {
                return;
            }
            FlagEvaluator.processCheck(checkEvent, deny -> {
                if (deny.getFlagCheck().getPlayer() != null) {
                    sendFlagMsg(deny);
                }
                super.fallOn(world, state, pos, trampler, fallDistance);
                ci.cancel();
            });

            if (trampler instanceof Player player) {
                checkEvent = new FlagCheckEvent(pos, TRAMPLE_FARMLAND_PLAYER, getDimKey(world), player);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    super.fallOn(world, state, pos, trampler, fallDistance);
                    ci.cancel();
                });
            } else {
                checkEvent = new FlagCheckEvent(pos, TRAMPLE_FARMLAND_OTHER, getDimKey(world), null);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    super.fallOn(world, state, pos, trampler, fallDistance);
                    ci.cancel();
                });
                checkEvent = new FlagCheckEvent(pos, MOB_GRIEFING, getDimKey(world), null);
                if (Services.EVENT.post(checkEvent)) {
                    return;
                }
                FlagEvaluator.processCheck(checkEvent, deny -> {
                    super.fallOn(world, state, pos, trampler, fallDistance);
                    ci.cancel();
                });
            }
        }
    }
}
