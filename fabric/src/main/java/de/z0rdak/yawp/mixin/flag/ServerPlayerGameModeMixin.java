package de.z0rdak.yawp.mixin.flag;

import net.minecraft.core.BlockPos;
import net.minecraft.network.protocol.Packet;
import net.minecraft.network.protocol.game.ClientGamePacketListener;
import net.minecraft.network.protocol.game.ClientboundBlockUpdatePacket;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.level.ServerPlayerGameMode;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.level.*;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import de.z0rdak.yawp.handler.flags.ExplosionDamageCalculatorInterceptor;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.*;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ServerPlayerGameMode.class)
public abstract class ServerPlayerGameModeMixin {
    @Shadow
    @Final
    protected ServerLevel level;
    @Shadow @Final protected ServerPlayer player;
    @Shadow protected GameType gameModeForPlayer;


    @Inject(method = "destroyBlock", at = @At("HEAD"), cancellable = true)
    private void fabric_onDestroyBlockHead(BlockPos pos, CallbackInfoReturnable<Boolean> cir) {
        BlockState state = this.level.getBlockState(pos);

        // Call your Fabric BlockBreakCallback (Forge-like)
        BlockBreakResult result = BlockBreakCallback.EVENT.invoker()
                .onBreak(this.player, this.level, pos, state, false);

        if (result.canceled()) {
            // Resync block to client like Forge
            this.player.connection.send(new ClientboundBlockUpdatePacket(this.level, pos));
            BlockEntity be = this.level.getBlockEntity(pos);
            if (be != null) {
                Packet<ClientGamePacketListener> pkt = be.getUpdatePacket();
                if (pkt != null) this.player.connection.send(pkt);
            }
            cir.setReturnValue(false);
            return;
        }

        result.xp();
    }

}
