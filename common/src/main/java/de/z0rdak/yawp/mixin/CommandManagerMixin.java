package de.z0rdak.yawp.mixin;

import com.mojang.brigadier.ParseResults;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.handler.CommandInterceptor;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.level.ServerPlayer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.core.flag.RegionFlag.EXECUTE_COMMAND;
import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(Commands.class)
public abstract class CommandManagerMixin {

    @Inject(method = "performCommand", at = @At(value = "HEAD"), cancellable = true)
    public void execute(ParseResults<CommandSourceStack> parseResults, String command, CallbackInfoReturnable<Integer> cir) {
        // check mod command permissions
        int result = 0;
        try {
            result = CommandInterceptor.handleModCommands(parseResults, command);
        } catch (CommandSyntaxException e) {
            throw new RuntimeException(e);
        }
        if (result != 0) {
            cir.setReturnValue(1);
        }
        // check exec-command flag
        CommandSourceStack cmdSource = parseResults.getContext().getSource();
        if (cmdSource.isPlayer()) {
            ServerPlayer player = cmdSource.getPlayer();
            if (player != null) {
                FlagCheckRequest checkEvent = new FlagCheckRequest(player.blockPosition(), EXECUTE_COMMAND, getDimKey(player), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(1);
                });
            }
        }
    }
}