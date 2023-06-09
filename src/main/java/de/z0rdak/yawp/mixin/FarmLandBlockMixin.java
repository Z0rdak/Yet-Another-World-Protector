package de.z0rdak.yawp.mixin;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.handler.flags.HandlerUtil;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
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

import static de.z0rdak.yawp.core.flag.RegionFlag.TRAMPLE_FARMLAND;
import static de.z0rdak.yawp.core.flag.RegionFlag.TRAMPLE_FARMLAND_OTHER;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.checkPlayerEvent;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.sendFlagDeniedMsg;

@Mixin(FarmlandBlock.class)
public abstract class FarmLandBlockMixin extends Block {

    // Dummy constructor needed to allow inheriting from Block, which in turn is needed to call super.onLandedUpon()
    // This constructor is not actually injected.
    public FarmLandBlockMixin(AbstractBlock.Settings settings) {
        super(settings);
    }

    @Inject(method = "onLandedUpon", at = @At(value = "HEAD"), cancellable = true)
    private void onTrampleFarmland(World world, BlockState state, BlockPos pos, Entity trampler, float fallDistance, CallbackInfo info) {
        if (!world.isClient) {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(world.getRegistryKey());
            FlagCheckEvent flagCheckEvent = HandlerUtil.checkTargetEvent(pos, TRAMPLE_FARMLAND, dimCache.getDimensionalRegion());
            if (flagCheckEvent.isDenied()) {
                if (trampler instanceof PlayerEntity player) {
                    sendFlagDeniedMsg(flagCheckEvent, player);
                }
                super.onLandedUpon(world, state, pos, trampler, fallDistance);
                info.cancel();
                return;
            }
            if (trampler instanceof PlayerEntity player) {
                FlagCheckEvent.PlayerFlagEvent playerFlagCheckEvent = checkPlayerEvent(player, pos, RegionFlag.TRAMPLE_FARMLAND_PLAYER, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    sendFlagDeniedMsg(playerFlagCheckEvent);
                    super.onLandedUpon(world, state, pos, trampler, fallDistance);
                    info.cancel();
                }
            } else {
                flagCheckEvent = HandlerUtil.checkTargetEvent(pos, TRAMPLE_FARMLAND_OTHER, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    super.onLandedUpon(world, state, pos, trampler, fallDistance);
                    info.cancel();
                }
                flagCheckEvent = HandlerUtil.checkTargetEvent(pos, MOB_GRIEFING, dimCache.getDimensionalRegion());
                if (flagCheckEvent.isDenied()) {
                    super.onLandedUpon(world, state, pos, trampler, fallDistance);
                    info.cancel();
                }
            }
        }
    }
}
