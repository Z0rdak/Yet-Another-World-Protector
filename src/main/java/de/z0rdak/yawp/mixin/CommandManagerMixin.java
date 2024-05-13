package de.z0rdak.yawp.mixin;

import com.mojang.brigadier.ParseResults;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.CommandInterceptor;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(CommandManager.class)
public abstract class CommandManagerMixin {

    @Inject(method = "execute", at = @At(value = "HEAD"), cancellable = true)
    public void execute(ParseResults<ServerCommandSource> parseResults, String command, CallbackInfoReturnable<Integer> cir) {
        // check mod command permissions
        int result = CommandInterceptor.handleModCommands(parseResults, command);
        if (result != 0) {
            cir.setReturnValue(1);
        }

        // check exec-command flag
        ServerCommandSource cmdSource = parseResults.getContext().getSource();
        if (cmdSource.isExecutedByPlayer()) {
            ServerPlayerEntity player = cmdSource.getPlayer();
            if (player != null) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, player.getBlockPos(), RegionFlag.EXECUTE_COMMAND, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    sendFlagDeniedMsg(flagCheck);
                    cir.setReturnValue(1);
                }
            }
        }
    }
}