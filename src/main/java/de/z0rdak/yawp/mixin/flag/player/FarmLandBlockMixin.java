package de.z0rdak.yawp.mixin.flag.player;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import net.minecraft.block.AbstractBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.FarmlandBlock;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.*;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;
import static de.z0rdak.yawp.util.MessageSender.sendFlagMsg;

@Mixin(FarmlandBlock.class)
public abstract class FarmLandBlockMixin extends Block {

    // Dummy constructor needed to allow inheriting from Block, which in turn is needed to call super.onLandedUpon()
    // This constructor is not actually injected.
    public FarmLandBlockMixin(AbstractBlock.Settings settings) {
        super(settings);
    }

    @Inject(method = "onLandedUpon", at = @At(value = "HEAD"), cancellable = true)
    private void onTrampleFarmland(World world, BlockState state, BlockPos pos, Entity trampler, float fallDistance, CallbackInfo ci) {
        if (isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, TRAMPLE_FARMLAND, getDimKey(world), null);
            if (post(checkEvent)) {
                return;
            }
            processCheck(checkEvent, null, deny -> {
                if (deny.getFlagCheck().getPlayer() != null) {
                    sendFlagMsg(deny);
                }
                super.onLandedUpon(world, state, pos, trampler, fallDistance);
                ci.cancel();
            });

            if (trampler instanceof PlayerEntity player) {
                checkEvent = new FlagCheckEvent(pos, TRAMPLE_FARMLAND_PLAYER, getDimKey(world), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    sendFlagMsg(deny);
                    super.onLandedUpon(world, state, pos, trampler, fallDistance);
                    ci.cancel();
                });
            } else {
                checkEvent = new FlagCheckEvent(pos, TRAMPLE_FARMLAND_OTHER, getDimKey(world), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    super.onLandedUpon(world, state, pos, trampler, fallDistance);
                    ci.cancel();
                });
                checkEvent = new FlagCheckEvent(pos, MOB_GRIEFING, getDimKey(world), null);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    super.onLandedUpon(world, state, pos, trampler, fallDistance);
                    ci.cancel();
                });
            }
        }
    }
}
